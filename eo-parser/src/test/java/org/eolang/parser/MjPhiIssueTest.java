package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test to reproduce the issue with MjPhi generating wrong phi expressions.
 */
final class MjPhiIssueTest {

    @Test
    void testAttributeNameWithDollarSign() throws Exception {
        String xmir = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<object>",
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
        System.out.println("Generated PHI:");
        System.out.println(phi);
        
        // Now also try the salty version which might be the culprit
        String saltyPhi = new Xmir(new XMLDocument(xmir)).toSaltyPhi();
        System.out.println("\nGenerated Salty PHI:");
        System.out.println(saltyPhi);
        
        // The issue is that we have "Φ.j$foo" as an attribute name when it should be just "j$foo"
        // But first let's check if we can reproduce the issue
        if (phi.contains("Φ.j$foo") || saltyPhi.contains("Φ.j$foo")) {
            System.out.println("ISSUE REPRODUCED: Found 'Φ.j$foo' in the phi expression");
        } else {
            System.out.println("Issue NOT reproduced, no 'Φ.j$foo' found");
        }
        
        Assertions.assertFalse(
            phi.contains("Φ.j$foo") || saltyPhi.contains("Φ.j$foo"),
            "PHI expression should not contain 'Φ.j$foo' as an attribute name"
        );
    }
}