Changelog
=========

### PR #5 Basic

* Add Changelog
* Implement the subject version of the Bsp in `Bsp_classic` module.
* Add functor and user interface to select play mode

### PR #8 Factorise function between bsp modules

* Reduce `Bsp_type` to minimal implementation
* Add iterators to `Bsp_type`
* Add Functor `Bsp_complete` implementing generic function based on a `Bsp_type`
* Line coloration
* Use `line_label` and `region_label` in Bsp for more modularity and readability

### PR #11 Implement help & check functions, find fnc, use sat solver

* Implements handlers for _Help_ and _Exists solution_ buttons
* Generate fnc representing the game status and solve with the sat solver
* Fix line coloration

### PR #17 User interface improvements

* Seperate view code in differents folders and files for more readability
* Fix issue #16, Detect when the player win the game and show a new screen
* Fix issue #15, Add more options on the main page
