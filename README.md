# sfdx-mode

*sfdx-mode** is an Emacs minor mode for working
with [[Salesforce DX projects][https://developer.salesforce.com/docs/atlas.en-us.sfdx_dev.meta/sfdx_dev/sfdx_dev_intro.htm]].

## Installation

### Manual Installation

Start by cloning the *sfdx-mode* package repository:

`$ git clone https://github.com/rody/sfdx-mode.git /your/path/here`

Finish by loading the *sfdx-mode* package in your emacs configuration:

```
(add-to-list 'load-path "/your/path/here/sfdx-mode")
(require 'sfdx-mode)
```

## usage

### Command Keymap

TODO

### sfdx-mode-visit-project-file

Running this command in a project directory will
visit the project file (sfdx-project.json) in a buffer.

### sfdx-mode-org-list

Running <C-c s o l> will list all the orgs you've created
or authenticated to
