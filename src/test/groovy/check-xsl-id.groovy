/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

/**
 * This check verifies that 'id' of XSL is in line with file name.
 */
import groovy.xml.XmlSlurper
import groovy.io.FileType
import groovy.io.FileVisitResult

File project = new File('.')

project.traverse(
    type: FileType.FILES,
    preDir: { file ->
        if (file.name == 'target') {
            return FileVisitResult.SKIP_SUBTREE
        }
    },
    nameFilter: ~/.*\.xsl|xs3p.xsl/,
) {
    file ->
    String id = new XmlSlurper().parse(file).@id
    assert id == file.name - '.xsl'
    log.info("The XSL stylesheet has correct @id: ${file}")
}
true
