import java.nio.file.Files
import java.util.stream.Collectors

/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

def classes = basedir.toPath().resolve("target").resolve("classes")
def testClasses = basedir.toPath().resolve("target").resolve("test-classes")
def binaries = Files.walk(classes).filter(Files::isRegularFile)
  .filter(file -> file.toString().endsWith(".class")).map {
  return classes.relativize(it).toString()
}.collect(Collectors.toSet())
def disjoint = Files.walk(testClasses).filter(Files::isRegularFile)
  .filter(file -> file.toString().endsWith(".class")).map {
  return testClasses.relativize(it).toString()
}.noneMatch { binaries.contains(it) }
println "Compiled classes do not have duplicates: " + disjoint
assert disjoint
return true