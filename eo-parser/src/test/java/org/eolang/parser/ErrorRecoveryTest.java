package org.eolang.parser;

import com.jcabi.xml.XML;
import org.cactoos.io.InputOf;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test for error recovery in parser.
 */
final class ErrorRecoveryTest {

    @Test
    void testErrorRecovery() throws Exception {
        String code = "# Example.\n[] > example\n  [x] +++ bad\n    one\n      two\n  [] > good\n    one\n      two";
        
        XML xml = new EoSyntax(new InputOf(code)).parsed();
        System.out.println("Generated XMIR:");
        System.out.println(xml.toString());
        
        // Check that we have errors
        System.out.println("\nErrors:");
        System.out.println(xml.xpath("/object/errors/error/text()"));
        
        // Check objects count - should be 2 (example and good), not 0
        long objectCount = Long.parseLong(xml.xpath("count(//o[@name])").get(0));
        System.out.println("\nObjects count: " + objectCount);
        
        // The test should fail initially since no objects are parsed after error
        // After fix, we should have at least the "good" object 
        Assertions.assertTrue(objectCount > 1, "Should recover and parse objects after error");
    }
}