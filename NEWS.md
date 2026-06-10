# bcwsft (development version)

* Initial scaffold: a faithful, arcpy-free R port of the BC Wildfire Service
  fuel-typing algorithm (Greene's `bcwft2018.py`; Perrakis et al. 2018). The
  derived-attribute predicates and the non-vegetated branch of the decision tree
  are ported; the vegetated-site branch is not yet translated and raises an error.
* `bcwsft_fuel_type()` assigns FBP fuel types to a data frame of VRI records.
* `bcwsft_fuel_type_one()` assigns an FBP fuel type to a single VRI record.
