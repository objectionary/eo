/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
 * The goal of the test is to check that the parameters passed to java are named
 * consistently. For example, "eo.outputDir".
 */

import groovy.xml.XmlSlurper
import groovy.xml.slurpersupport.GPathResult

import java.nio.file.Path

Path plugin = basedir.toPath()
    .resolve('target')
    .resolve('classes')
    .resolve('META-INF')
    .resolve('maven')
    .resolve('plugin.xml')
GPathResult content = new XmlSlurper().parseText(new File(plugin.toString()).text)
// For example, "${eo.foreignFormat}":
String pattern = "\\\$\\{eo\\.[a-z]+([A-Z][a-z]+)*}"
List<String> failures = []
List<String> toBeExcluded = ['help']

content.mojos.mojo.findAll { mojo -> !(mojo.goal.text() in toBeExcluded) }
    .configuration
    .each { mojoConf ->
        mojoConf.children().each { child ->
            final String text = child.text()
            if (!(text == '' || text.matches(pattern))) {
                failures.add(text)
            }
        }
    }
if (!failures.empty) {
    fail("Following parameters don\'t match pattern ${pattern}:\n ${failures.join(' ')}")
}
true
