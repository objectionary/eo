/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
