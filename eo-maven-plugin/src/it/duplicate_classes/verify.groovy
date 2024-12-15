import java.nio.file.Files
import java.nio.file.Path
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

String target = 'target'
String classExtension = '.class'

Path classes = basedir.toPath().resolve(target).resolve('classes')
Path testClasses = basedir.toPath().resolve(target).resolve('test-classes')
Set<String> binaries = Files.walk(classes).filter(Files::isRegularFile)
        .filter(file -> file.toString().endsWith(classExtension))
        .map { path -> classes.relativize(path).toString() }
        .collect(Collectors.toSet())
boolean disjoint = Files.walk(testClasses).filter(Files::isRegularFile)
        .filter(file -> file.toString().endsWith(classExtension))
        .map { path -> testClasses.relativize(path).toString() }
        .noneMatch { classPathName -> binaries.contains(classPathName) }

log.info("Compiled classes do not have duplicates: $disjoint")
assert disjoint
return true
