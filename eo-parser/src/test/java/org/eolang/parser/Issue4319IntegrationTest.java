/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Integration test for the fix to issue #4319.
 */
final class Issue4319IntegrationTest {

    @Test
    void testJavaNamesDoNotGetPhiPrefix() throws Exception {
        String xmir = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<object>",
            "  <metas>",
            "    <meta>",
            "      <head>alias</head>",
            "      <tail>j$foo</tail>",
            "      <part>j$foo</part>",
            "    </meta>",
            "  </metas>",
            "  <o name='j$foo'>",
            "    <o name='j$AbstractParent' base='Q.jeo.class'>",
            "      <o name='j$foo' base='Q.jeo.method'>",
            "        <o name='signature'>\"\"</o>",
            "      </o>",
            "    </o>",
            "  </o>",
            "</object>"
        );
        
        String phi = new Xmir(new XMLDocument(xmir)).toPhi();
        String saltyPhi = new Xmir(new XMLDocument(xmir)).toSaltyPhi();
        
        // Verify that j$ names don't get the Φ. prefix
        Assertions.assertFalse(
            phi.contains("Φ.j$foo"),
            "Sweet PHI should not contain 'Φ.j$foo'"
        );
        
        Assertions.assertFalse(
            saltyPhi.contains("Φ.j$foo"),
            "Salty PHI should not contain 'Φ.j$foo'"
        );
        
        // Verify the correct structure
        Assertions.assertTrue(
            phi.contains("j$foo ↦ ⟦"),
            "PHI should contain 'j$foo ↦ ⟦'"
        );
        
        Assertions.assertTrue(
            phi.contains("j$foo ↦ Φ.jeo.method"),
            "PHI should contain 'j$foo ↦ Φ.jeo.method'"
        );
    }
}