# bcwsft: BC Wildfire Service Fuel Typing Algorithm (R port)

An R port of the British Columbia Wildfire Service fuel-typing algorithm
(Perrakis, Eade & Hicks 2018), translated faithfully from Gregory A.
Greene's Python implementation `bcwft2018`. It assigns Canadian Forest
Fire Behaviour Prediction (FBP) System fuel types to VRI polygons from
forest-inventory attributes, with no ArcGIS dependency.

## License

GPL-3, as a derivative of GPL-3 source. See `LICENSE.md`.

## Provenance

Original Python:
<https://github.com/gagreene/BC_CFFBPS_FuelTyping_Tool>. Each ported
block carries a comment pointing back to the corresponding
`bcwft2018.py` line range so the translation can be audited.

## See also

Useful links:

- <https://github.com/gagreene/BC_CFFBPS_FuelTyping_Tool>

## Author

**Maintainer**: Alex M. Chubaty <achubaty@for-cast.ca> (R port)

Authors:

- Alex M. Chubaty <achubaty@for-cast.ca> (R port)

- Gregory A. Greene <map.n.trowel@gmail.com> (Author of the original
  Python implementation (bcwft2018)) \[copyright holder\]
