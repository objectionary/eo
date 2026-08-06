/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * This check verifies that stylesheet-only namespaces are excluded
 * from literal result elements.
 */
import groovy.io.FileType
import groovy.io.FileVisitResult
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Element

File project = new File('.')
DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance()
factory.namespaceAware = true

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
    Element stylesheet = factory.newDocumentBuilder().parse(file).documentElement
    Set<String> excluded = stylesheet
        .getAttribute('exclude-result-prefixes')
        .tokenize()
        .toSet()
    ['eo', 'xs'].each {
        prefix ->
        if (stylesheet.lookupNamespaceURI(prefix) != null) {
            assert excluded.contains(prefix) || excluded.contains('#all') :
                "${file} declares '${prefix}' but does not exclude it from results"
        }
    }
}
true
