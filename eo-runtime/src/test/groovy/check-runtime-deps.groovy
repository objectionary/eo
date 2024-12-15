/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation directories (the "Software"), to deal
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
 * The goal of the test is to check that eo-runtime does not contain any dependencies except those
 * that are needed for tests. We need such behaviour because "eo-runtime" is download as separated
 * jar (not fat-jar) and we expect that it won't require any outer dependencies
 */
import groovy.xml.XmlSlurper
import groovy.xml.slurpersupport.GPathResult

String pom = new File('pom.xml').text
GPathResult project = new XmlSlurper().parseText(pom)

project.dependencies.dependency.each { dependency ->
    if (dependency.scope.text() != 'test' && dependency.scope.text() != 'provided') {
        fail(
            "Dependency ${dependency.groupId.text()}.${dependency.artifactId.text()} " +
            "must be in either 'test' or 'provided' scope"
        )
    }
}
