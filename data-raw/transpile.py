#!/usr/bin/env python3
"""Transpile bcwft2018.py FuelTyping.decisionTree (Python) -> an R function body.

The tree is a regular nested if/elif/else returning (lineno, fuel, modifier).
We emit an R `bcwsft_decision_tree(s)` that mirrors it. Branch labels use the
original Python source line number (golden validation compares fuel_type +
modifier, not the label).
"""
import re
import sys

SRC = sys.argv[1] if len(sys.argv) > 1 else "bcwft2018.py"
lines = open(SRC).read().split("\n")

# --- locate decisionTree body ---------------------------------------------
start = next(i for i, l in enumerate(lines) if l.strip() == "def decisionTree(self):")
end = next(i for i in range(start + 1, len(lines)) if re.match(r"    def \w+\(", lines[i]))
body = lines[start + 1 : end]
# drop the docstring (first non-blank block of triple-quoted text)
while body and body[0].strip() == "":
    body.pop(0)
if body and body[0].lstrip().startswith('"""'):
    body.pop(0)
    while body and '"""' not in body[0]:
        body.pop(0)
    if body:
        body.pop(0)

# --- collapse the multi-line inspect.lineno return into a marker -----------
text = "\n".join(body)
text = re.sub(r"inspect\.getframeinfo\(\s*inspect\.currentframe\(\)\s*\)\.lineno", "LN", text)
body = text.split("\n")

# attach original line numbers (1-based in the file)
numbered = [(start + 1 + 1 + i, l) for i, l in enumerate(body)]  # approx; refined below
# recompute: body[i] corresponds to file line (start+1)+ (offset). Simpler: recompute via re-scan.

# --- join continuation lines (unbalanced parens/brackets) ------------------
def depth(s):
    d = 0
    instr = None
    for ch in s:
        if instr:
            if ch == instr:
                instr = None
            continue
        if ch in "'\"":
            instr = ch
        elif ch in "([":
            d += 1
        elif ch in ")]":
            d -= 1
    return d

logical = []  # (indent, lineno_label, text)
i = 0
fileline = start + 2  # first body line's file line number (1-based)
# Build a parallel list of (fileline, raw) skipping what we popped is hard; instead recompute labels
# from the collapsed text by counting returns' position. We approximate label by running counter of
# original lines; good enough since labels are only for traceability.
raw = body
li = 0
linenum_base = 0
while li < len(raw):
    line = raw[li]
    if line.strip() == "" or line.strip().startswith("#"):
        logical.append((len(line) - len(line.lstrip()), None, line.strip(), "comment_or_blank"))
        li += 1
        continue
    indent = len(line) - len(line.lstrip())
    buf = line
    while depth(buf) > 0 and li + 1 < len(raw):
        li += 1
        buf += " " + raw[li].strip()
    code = re.sub(r"\s+#.*$", "", buf).strip()  # strip inline comment
    logical.append((indent, li, code, "code"))
    li += 1

# --- condition translation -------------------------------------------------
def xlate_cond(c):
    c = c.strip()
    if c.endswith(":"):
        c = c[:-1].strip()
    c = c.replace("self.", "s$")
    # membership against list attributes -> R constants
    c = re.sub(r"\bin\s+s\$coniferList\b", "%in% bcwsft_conifer_list", c)
    c = re.sub(r"\bin\s+s\$dryBECzones\b", "%in% bcwsft_dry_bec_zones", c)
    c = re.sub(r"\bin\s+s\$borealBECzones\b", "%in% bcwsft_boreal_bec_zones", c)
    # checkDomConifers([...]) -> bcwsft_check_dom_conifers(s, c(...))
    c = re.sub(r"s\$checkDomConifers\(\[(.*?)\]\)", r"bcwsft_check_dom_conifers(s, c(\1))", c)
    # str(X).startswith('Y') and X.startswith('Y') -> startsWith(...)
    c = re.sub(r"str\(\s*(s\$\w+)\s*\)\.startswith\(\s*'([^']*)'\s*\)", r"startsWith(as.character(\1), '\2')", c)
    c = re.sub(r"(s\$\w+)\.startswith\(\s*'([^']*)'\s*\)", r"startsWith(\1, '\2')", c)
    # protect is.na targets later; first map None comparisons
    c = re.sub(r"(s\$\w+)\s+is not None", r"!is.na(\1)", c)
    c = re.sub(r"(s\$\w+)\s+is None", r"is.na(\1)", c)
    # membership
    c = re.sub(r"(s\$\w+)\s+not in\s+\[(.*?)\]", r"!(\1 %in% c(\2))", c)
    c = re.sub(r"(s\$\w+)\s+in\s+\[(.*?)\]", r"(\1 %in% c(\2))", c)
    # boolean attrs that may be NA -> isTRUE (protect is.na(...) occurrences)
    c = c.replace("is.na(s$is_vegetated)", "@@NAVEG@@")
    for b in ("is_vegetated", "is_forested", "is_logged", "is_burned"):
        c = re.sub(r"(?<![\w$])s\$" + b + r"(?![\w])", "isTRUE(s$" + b + ")", c)
    c = c.replace("@@NAVEG@@", "is.na(s$is_vegetated)")
    # NA-safe equality (Python None == x -> False, None != x -> True);
    # ordering comparisons (< <= > >=) left as-is (Python None<x crashes, R NA<x errors -> both fail).
    c = c.replace("!=", "%ne%").replace("==", "%eq%")
    c = c.replace(" and ", " && ").replace(" or ", " || ")
    c = re.sub(r"\bnot\s+", "!", c)
    c = re.sub(r"\bNone\b", "NA", c)  # list-literal None -> NA (membership stays correct)
    return c

def xlate_return(stmt, label):
    m = re.search(r"return\s*\(?\s*LN\s*,\s*'([^']*)'\s*,\s*(.+)$", stmt)
    if not m:
        return f'stop("UNPARSED RETURN: {stmt}")'
    fuel = m.group(1)
    mod = m.group(2).strip().rstrip(")").strip()
    lab = f"L{label}" if label is not None else "L?"
    if mod == "None":
        return f'return(.ft("{lab}", "{fuel}"))'
    mod = mod.replace("self.", "s$")
    return f'return(.ft("{lab}", "{fuel}", {mod}))'

# --- emit R with an indentation stack --------------------------------------
out = []
stack = []  # indents with an open brace
def close_to(indent):
    while stack and stack[-1] >= indent:
        stack.pop()
        out.append("  " * len(stack) + "}")

for indent, label, txt, kind in logical:
    if kind == "comment_or_blank":
        if txt.startswith("#"):
            out.append("  " * len(stack) + "## " + txt.lstrip("# ").rstrip())
        continue
    close_to(indent)
    pad = "  " * len(stack)
    if txt.startswith("if "):
        out.append(f"{pad}if ({xlate_cond(txt[3:])}) {{")
        stack.append(indent)
    elif txt.startswith("elif "):
        out.append(f"{pad}else if ({xlate_cond(txt[5:])}) {{")
        stack.append(indent)
    elif txt.startswith("else"):
        out.append(f"{pad}else {{")
        stack.append(indent)
    elif txt.startswith("return"):
        out.append(pad + xlate_return(txt, label))
    else:
        out.append(f'{pad}stop("UNPARSED: {txt}")')
close_to(-1)

print("bcwsft_decision_tree <- function(s) {")
for l in out:
    print("  " + l)
print('  .unmatched("py-fallthrough")')
print("}")
