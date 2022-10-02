<img src="https://rawgithub.com/yegor256/elegantobjects/master/cactus.svg" height="100px"/>

[![make](https://github.com/objectionary/eo/actions/workflows/latexmk.yml/badge.svg)](https://github.com/objectionary/eo/actions/workflows/latexmk.yml)

This is a more or less formal description of EOLANG and φ-calculus.

To build it, just run:

```bash
$ make
```

You need to have
[LaTeX](https://www.latex-project.org/get/),
[`aspell`](http://aspell.net/),
[`jq`](https://stedolan.github.io/jq/),
[`biblint`](https://github.com/Kingsford-Group/biblint),
[`texsc`](https://rubygems.org/gems/texsc),
and
[`texqc`](https://rubygems.org/gems/texqc)
installed.

You may also want to check whether your LaTeX installation has
all the packages required [in here](https://github.com/objectionary/eo/blob/master/.github/workflows/latexmk.yml).
If you miss any of them, install, for example, with the help of
[`tlmgr`](https://tug.org/texlive/tlmgr.html).

Once compiled and packaged, upload zip archive to [arXiv](https://arxiv.org/abs/2111.13384).
