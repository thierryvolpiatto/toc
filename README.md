<!-- markdown-toc start -->
**Table of Contents**

- [toc](#toc)
- [Install](#install)
- [Usage](#usage)

<!-- markdown-toc end -->

# toc

Generate github flavor table of contents for org/mardown files.
This is used for the Helm wiki and homepage maintainance.

# Install
Put toc.el in your `load-path` and add in your init file:

    (autoload 'toc-insert-headers-at-point "toc" nil t)
    (autoload 'toc-toc "toc" nil t)

# Usage

On top of your file either markdown or org, install TOC headers with
M-x toc-insert-headers-at-point.
Then run M-x toc-toc.
