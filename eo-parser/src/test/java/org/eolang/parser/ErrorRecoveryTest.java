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
        System.out.println("\nErrors:");
        System.out.println(xml.xpath("/object/errors/error/text()"));
        long objectCount = Long.parseLong(xml.xpath("count(//o[@name])").get(0));
        System.out.println("\nObjects count: " + objectCount);
        long goodCount = Long.parseLong(xml.xpath("count(//o[@name='example']/o[@name='good'])").get(0));
        System.out.println("Good objects inside example: " + goodCount);
        String listing = xml.xpath("/object/listing/text()").get(0);
        System.out.println("Listing length: " + listing.length());
        Assertions.assertTrue(listing.contains("good"), "Should preserve full listing including good object");
    }

    @Test
    void testSimpleErrorRecovery() throws Exception {
        String code = "# Simple test.\n[] > obj\n  [x] +++ bad\n  [] > good";
        XML xml = new EoSyntax(new InputOf(code)).parsed();
        System.out.println("Simple test XMIR:");
        System.out.println(xml.toString());
        String listing = xml.xpath("/object/listing/text()").get(0);
        System.out.println("Simple listing: " + listing);
        Assertions.assertTrue(listing.contains("good"), "Should preserve full listing in simple case");
    }
}