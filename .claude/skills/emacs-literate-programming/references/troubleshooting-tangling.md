# Troubleshooting Org-Babel Tangling Issues

This guide provides systematic troubleshooting steps for when auto-tangling or manual tangling fails.

## Common Issue: Property Line Not Activated

**Symptom**: Saving `.org` file with `#+auto_tangle: y` doesn't update the tangled file.

**Root Cause**: The `#+PROPERTY: header-args` line was never activated in Emacs.

**Solution**:
1. Open the `.org` file in Emacs
2. Navigate to the `#+PROPERTY: header-args:...` line (usually line 3-4 near top)
3. Place cursor on that line
4. Press `C-c C-c` to activate the property
5. You should see message: "Local setup has been refreshed"
6. Save the file - tangling should now work

**Why this happens**: Property lines in org-mode don't take effect until explicitly activated. This is documented in [org-mode mailing lists](https://lists.gnu.org/archive/html//emacs-orgmode/2020-03/msg00271.html) as a frequently missed step that causes silent failures.

**Alternative activation methods**:
- Restart org-mode: `M-x org-mode-restart`
- Reopen the buffer: Close and reopen the file

## Systematic Debugging Process

When tangling fails, follow these steps in order:

### 1. Check if org-auto-tangle-mode is Active

```elisp
M-x describe-mode
```

Look for "Org-Auto-Tangle" in the list of enabled minor modes.

**If not present**:
- Run `M-x org-auto-tangle-mode` to enable it manually
- Check if the mode is configured correctly in `emacs/major-modes/org.org`

### 2. Verify Package is Loaded

```elisp
C-h v org-auto-tangle-mode
```

Should display the variable documentation. If it says "void variable", the package isn't installed or loaded.

### 3. Test Manual Tangling

```elisp
M-x org-babel-tangle
```

**If manual tangling works but auto-tangle doesn't**:
- The issue is with org-auto-tangle specifically
- Check if `#+auto_tangle: y` is present in the file
- Any non-nil value works: `y`, `t`, `yes`

**If manual tangling also fails**:
- The issue is with org-babel-tangle itself
- Check the Messages buffer for errors (see step 4)
- Verify header-args syntax is correct

### 4. Check Messages Buffer for Errors

```elisp
C-h e  (or switch to *Messages* buffer)
```

Look for error messages after saving. Common errors:
- File permission issues
- Invalid header arguments syntax
- Missing directories in tangle path

### 5. Verify Property Activation

```elisp
C-c C-c on the #+PROPERTY line
```

Should see: "Local setup has been refreshed"

If you don't see this message, the property line may have syntax errors.

### 6. Inspect Active Header Arguments

To see what header arguments are actually active for a specific source block:

```elisp
# Place cursor in the source block
C-c C-v C-i  (org-babel-view-src-block-info)
```

This displays all active header arguments for that block, showing you the full precedence resolution.

## Understanding Multiple Tangle Directives

When a `.org` file has multiple tangle directives, they follow this precedence hierarchy:

### Precedence Order (highest to lowest)

1. **Block-level** (highest priority)
   - Specified on `#+begin_src` line: `#+begin_src sh :tangle no`
   - Overrides all other settings

2. **Subtree-level** (middle priority)
   - Specified in `:PROPERTIES:` drawer under a heading
   - Example:
     ```org
     * Section Name
     :PROPERTIES:
     :header-args:conf: :tangle .zsh_plugins.txt :comments no
     :END:
     ```
   - Overrides document-level settings

3. **Document-level** (lowest priority)
   - Specified at top of file: `#+PROPERTY: header-args:sh :tangle zshrc`
   - Default for all blocks of that language

### Language-Specific Headers

You can have different settings for different languages:

```org
#+PROPERTY: header-args:sh :tangle script.sh :comments both
#+PROPERTY: header-args:emacs-lisp :tangle init.el
#+PROPERTY: header-args:python :tangle no
```

Language-specific headers (`header-args:LANG`) work at both document and subtree levels.

### Common Pattern Example

```org
#+PROPERTY: header-args:sh :tangle ~/config.sh :comments both

* Installation Instructions
:PROPERTIES:
:header-args:sh: :tangle no
:END:

#+begin_src sh
# This won't be tangled (subtree overrides document)
brew install something
#+end_src

* Runtime Configuration

#+begin_src sh
# This WILL be tangled to ~/config.sh (uses document default)
export PATH="$HOME/bin:$PATH"
#+end_src

#+begin_src sh :tangle /tmp/test.sh
# This tangles to /tmp/test.sh (block overrides everything)
echo "test"
#+end_src
```

## Path Resolution

### Relative vs Absolute Paths

Both are valid and fully supported:

**Relative paths** (recommended for consistency):
```org
#+PROPERTY: header-args:emacs-lisp :tangle init.el
```
Tangled file created at: `./init.el` (relative to .org file location)

**Absolute paths**:
```org
#+PROPERTY: header-args:sh :tangle ~/src/dotfiles/zshrc
```
Tangled file created at: `~/src/dotfiles/zshrc`

**Default behavior**: When no `:tangle` argument specified, org-mode uses the .org filename with extension from source language.

## Quick Reference: Header Args Syntax

### Correct Syntax (current)
```org
#+PROPERTY: header-args :tangle yes
#+PROPERTY: header-args:sh :tangle script.sh :comments both
```

### Obsolete Syntax (deprecated 2013)
```org
#+PROPERTY: tangle yes    # Wrong - missing "header-args"
```

Always include `header-args` or `header-args:LANG` after `#+PROPERTY:`.

## Additional Notes

### auto_tangle Values

All these values are equivalent (any non-nil value works):
- `#+auto_tangle: y`
- `#+auto_tangle: t`
- `#+auto_tangle: yes`

Only `#+auto_tangle: nil` disables auto-tangling.

### Async Tangling Limitations

org-auto-tangle works asynchronously to avoid blocking Emacs. This means:
- No interactive prompts (like password requests) will work
- TRAMP paths may require cached credentials
- Complex noweb expansion may need configuration

For files requiring interactive input, use manual tangling: `M-x org-babel-tangle`

## Resources

- [Org Manual: Using Header Arguments](https://orgmode.org/manual/Using-Header-Arguments.html) - Official documentation on precedence
- [Org Manual: Extracting Source Code](https://orgmode.org/manual/Extracting-Source-Code.html) - Official tangling documentation
- [org-auto-tangle GitHub](https://github.com/yilkalargaw/org-auto-tangle) - Package repository and issues
- [Org-mode mailing list: Property activation issue](https://lists.gnu.org/archive/html//emacs-orgmode/2020-03/msg00271.html) - Discussion of C-c C-c requirement
