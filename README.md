# Collection of functions used in Master's Thesis

mafun is an R-package that provides a clean overview over the functions I wrote during my Master's Thesis. It is mainly concentrated on the work with German political Twitter data.

## Installation

mafun is available on Github. To install packages from Github, the devtools-package is needed.


```
if(!require('devtools')) install.packages('devtools'); require(devtools)
```

You can then install mafun with the following code.

```
if(!require('mafun')) install_github('s0eren/mafun', 's0eren')
library(mafun)
```

## Included Functions

To get an overview of the included functions and a description of their application, use:

```
?mafun
```

## Copyright & License

Copyright (C) 2014 - Released under the MIT License.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.