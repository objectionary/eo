/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * This check verifies that version of XSL has expected value.
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
  nameFilter: ~/.*\.xsl/,
) {
  file ->
    if (file.name == 'xs3p.xsl') { return }
    String version = new XmlSlurper().parse(file).@version
    assert version == '2.0'
}

true
