/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
