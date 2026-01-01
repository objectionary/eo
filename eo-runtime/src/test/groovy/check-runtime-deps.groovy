/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
    if (dependency.scope == null || (dependency.scope.text() != 'test' && dependency.scope.text() != 'provided')) {
        fail(
            "Dependency ${dependency.groupId.text()}.${dependency.artifactId.text()} " +
            "must be in either 'test' or 'provided' scope"
        )
    }
}
