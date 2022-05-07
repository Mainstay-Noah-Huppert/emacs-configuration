# Emacs Configuration
Emacs configuration directory.

# Table Of Contents
- [Overview](#overview)
- [Setup](#setup)
- [Custom Functionality](#custom-functionality)

# Overview
Emacs configuration directory (`~/.emacs.d`).  

# Setup
1. Clone this repository to Emacs the configuration directory (`~/.emacs.d/`)
2. Install `use-package` in Emacs:
   ```emacs
   M-x package-refresh-contents
   M-x package-install use-package
   ```
3. Load the `init.el` file or relaunch emacs

# Custom Functionality
The `init.el` file and associated lisp provides some custom features on top of Emacs:

## Machine Specific Configuration
The `machines/` directory contains Lisp files which are only included on machines with matching names (determined using the `system-name` variable).

For example if a machine was named `foo-bar` then the `machines/foo-bar.el` file would be included if it exists.

This can be helpful for defining code which one only wants to run on certain machines, and for setting machine specific values (ex., screen sizes).

## Dark / Light Mode Toggle
The `my-dark-or-light` variable can be set to use a dark or light theme. A value of `light` enables a light theme, otherwise a dark theme is used.

## Screen Specific Font Sizes
> Note: Only available if the [machine specific file](#machine-specific-configuration) defines these functions. Currently true for the `apollo` and `metis` machines.

The `set-best-font-size` interactive function can be used to tweak the font size based on the current monitor. This allows for a smaller font to be used on lower resolution screens and a larger font to be used on higher resolution screens.

## VTerm Minibuffer (WIP)
This is a work in progress feature which tries to make a better version of ansi-term. Improvements include:

- Uses VTerm, a better Emacs terminal emulator
- Commands entered through a minibuffer (like `shell-command`) but whole terminal is emulated and displayed
- Tries to group terminals by source control project instead of strictly by directory
- Customizable split behavior

It is currently work in progress, see the source code comments near `vterm-minibuffer` within `init.el` for a todo list.

To use the functionality either invoke `vterm-minibuffer` or use the chord `M-!`.

To customize the split behavior set the `vterm-minibuffer-split-function` variable to a function which selects the window one wishes to place the VTerm buffer within and then returns this window. Currently the following functions are provided which implement this behavior:

- `vterm-minibuffer-split-window-other`: Uses the next window in the switch order for VTerm, if there is only one window open that window is used
- `vterm-minibuffer-split-window-next`: Ensures that there is at least one window split, splits windows using the function from the `vterm-minibuffer-split-window-next-split-function` variable to spit windows if they are not already, then window that is not the window the cursor was in when the function was invoked
  - `vterm-minibuffer-split-window-next-split-function` Is the variable used to split windows if they are not already, it should return the newly split window. Built in Emacs functions like `split-window-right`, `split-window-below`, `split-window-horizontally`, and` split-window-vertically` can be used
