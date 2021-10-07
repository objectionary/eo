/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

assert new File(basedir, 'target/eo-foreign.csv').exists()
assert new File(basedir, 'target/eo-resolved.csv').exists()

assert new File(basedir, 'target/generated-sources/EOorg/EOeolang/EOexamples/EOapp.java').exists()
assert new File(basedir, 'target/eo/01-parse/org/eolang/examples/app.eo.xml').exists()
assert new File(basedir, 'target/eo/02-steps/org/eolang/examples/app/00-not-empty-atoms.xml').exists()
assert new File(basedir, 'target/eo/03-optimize/org/eolang/examples/app.eo.xml').exists()
assert new File(basedir, 'target/eo/04-pull/org/eolang/array.eo').exists()
assert new File(basedir, 'target/eo/05-pre/org/eolang/examples/app/00-pre-classes.xml').exists()
assert new File(basedir, 'target/eo/06-transpile/org/eolang/examples/app.eo.xml').exists()
assert new File(basedir, 'target/classes/EOorg/EOeolang/EOexamples/EOapp.class').exists()
