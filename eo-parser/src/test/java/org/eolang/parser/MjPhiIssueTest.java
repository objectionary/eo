package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test to reproduce the issue with MjPhi generating wrong phi expressions.
 */
final class MjPhiIssueTest {

    @Test
    void testAttributeNameWithDollarSignInAlias() throws Exception {
        // Test case that reproduces the issue described in GitHub issue #4319
        // When j$foo is in aliases, it should NOT get the Φ. prefix
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
        System.out.println("Generated PHI with j$ alias:");
        System.out.println(phi);
        
        // The fix: j$foo should appear as a normal attribute name, not Φ.j$foo
        Assertions.assertFalse(
            phi.contains("Φ.j$foo"),
            "PHI expression should not contain 'Φ.j$foo' - j$ names should not get program prefix"
        );
        
        // Verify the correct structure
        Assertions.assertTrue(
            phi.contains("j$foo ↦ ⟦"),
            "PHI should contain 'j$foo ↦ ⟦' as the main object binding"
        );
        
        Assertions.assertTrue(
            phi.contains("j$foo ↦ Φ.jeo.method"),
            "PHI should contain 'j$foo ↦ Φ.jeo.method' inside the class"
        );
    }
    
    @Test
    void testRegularAliasStillGetsPrefix() throws Exception {
        // Test that regular aliases (not starting with j$) still get the Φ. prefix
        String xmir = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<object>",
            "  <metas>",
            "    <meta>",
            "      <head>alias</head>",
            "      <tail>myAlias</tail>",
            "      <part>myAlias</part>",
            "    </meta>",
            "  </metas>",
            "  <o name='myAlias'>",
            "    <o name='test' base='string'>hello</o>",
            "  </o>",
            "</object>"
        );
        
        String phi = new Xmir(new XMLDocument(xmir)).toPhi();
        System.out.println("Generated PHI with regular alias:");
        System.out.println(phi);
        
        // Regular aliases should still get the Φ. prefix
        Assertions.assertTrue(
            phi.contains("Φ.myAlias ↦ ⟦"),
            "PHI should contain 'Φ.myAlias ↦ ⟦' for regular aliases"
        );
    }
}