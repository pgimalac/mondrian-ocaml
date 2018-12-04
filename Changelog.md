Changelog
=========

## PR #5 Basic

* Add Changelog
* Implement the subject version of the Bsp in `Bsp_classic` module.
* Add functor and user interface to select play mode

### PR #8 Factorise function between bsp modules

* Reduce `Bsp_type` to minimal implementation
* Add iterators to `Bsp_type`
* Add Functor `Bsp_complete` implementing generic function based on a `Bsp_type`
* Line coloration
* Use `line_label` and `region_label` in Bsp for more modularity and readability
