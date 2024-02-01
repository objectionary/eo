/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.eolang.maven.util.HmBase;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link VerifyMojo}.
 *
 * @since 0.31.0
 * @todo #2674:30min The messages "Warnings identified" from
 *  /org/eolang/parser/fail-on-warnings.xsl
 *  can have nullable line number. Need fix it, that it works as in
 *  /org/eolang/parser/warnings/mandatory-version-meta.xsl and
 *  /org/eolang/parser/warnings/mandatory-home-meta.xsl.
 *  After you need fix {@code createRegEx()}.
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
class VerifyMojoTest {

    @Test
    void doesNotFailWithNoErrorsAndWarnings(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Verify()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    @CaptureLogs
    void detectsErrorsSuccessfully(
        @TempDir final Path temp,
        final Logs out) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "  QQ.io.stdout",
                    "    \"Hello world\""
                )
                .execute(new FakeMaven.Verify()),
            "Program with noname attributes should have failed or error, but it didn't"
        );
        final String message = this.getMessage(out, "Errors identified");
        Assertions.assertTrue(
            message.matches(this.createRegEx(temp, "Errors identified")),
            "Errors message should have program name and error line number"
        );
    }

    @Test
    @CaptureLogs
    void detectsCriticalErrorsSuccessfully(
        @TempDir final Path temp,
        final Logs out) throws Exception {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "    \"Hello world\""
                )
                .execute(new FakeMaven.Verify()),
            "Wrong program should have failed or error, but it didn't"
        );
        final String message = this.getMessage(out, "Critical error identified");
        Assertions.assertTrue(
            message.matches(this.createRegEx(temp, "Critical error identified")),
            "Critical error message should have program name and error line number"
        );
    }

    @Test
    @CaptureLogs
    void detectsWarningWithCorrespondingFlag(
        @TempDir final Path temp,
        final Logs out) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "  # This is the default 64+ symbols comment in front of named abstract object.",
                    "  [] > @",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", true)
                .execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should have failed on warning, but it didn't"
        );
        final String message = this.getMessage(out, "Warnings identified");
        Assertions.assertTrue(
            message.matches(this.createRegEx(temp, "Warnings identified")),
            "Warnings message should have program name and error line number"
        );
    }

    @Test
    void doesNotDetectWarningWithoutCorrespondingFlag(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "  # This is the default 64+ symbols comment in front of named abstract object.",
                    "  [] > @",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", false)
                .execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should not have failed on warning without flag, but it does"
        );
    }

    @Test
    void failsOptimizationOnError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .execute(new FakeMaven.Verify()),
            "Error in the eo code because of invalid alias, should fail"
        );
    }

    @Test
    void failsOptimizationOnCritical(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                ).with("trackOptimizationSteps", true)
                .execute(new FakeMaven.Verify())
        );
    }

    @Test
    void failsParsingOnError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram("something > is wrong here")
                .execute(new FakeMaven.Verify()),
            "Program with invalid syntax should have failed, but it didn't"
        );
    }

    @Test
    void failsOnInvalidProgram(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(AssembleMojoTest.INVALID_PROGRAM)
                .execute(new FakeMaven.Verify()),
                "Invalid program with wrong syntax should have failed to assemble, but it didn't"
        );
    }

    @Test
    void failsOnWarning(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+architect yegor256@gmail.com",
                "+tests",
                "+package org.eolang.examples\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "  # This is the default 64+ symbols comment in front of named abstract object.",
                "  [] > @",
                "    hello > test"
            );
        VerifyMojoTest.applyXsl(
            "org/eolang/maven/set-warning-severity.xsl",
            maven.execute(ParseMojo.class)
                .result()
                .get("target/1-parse/foo/x/main.xmir")
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.with("failOnWarning", true)
                .execute(VerifyMojo.class),
            "Program with warning should fail"
        );
    }

    /**
     * Apply XSL transformation.
     * @param xsl Path to XSL within classpath
     * @param xml Path to XML to be tranformed
     */
    private static void applyXsl(final String xsl, final Path xml) throws Exception {
        final XML output = new Xsline(
            new TrDefault<Shift>()
                .with(
                    new StXSL(
                        new XSLDocument(
                            new ResourceOf(xsl).stream()
                        )))
        ).pass(new XMLDocument(xml));
        new HmBase(xml.getParent()).save(output.toString(), xml.getParent().relativize(xml));
    }

    /**
     * Parse the error message to program name and error line number for checking.
     * @param logs Logs logs
     * @param error String needed error message
     */
    private String getMessage(final Logs logs, final String error) {
        return String.valueOf(logs.captured().stream()
            .filter(
                log -> log.contains(error)
            ).findFirst()
        );
    }

    /**
     * Create regular expression for testing.
     * @param path Path program
     * @param error String needed error message
     */
    private String createRegEx(final Path path, final String error) {
        final String str = ".*".concat(error)
            .concat(":\\s{3}(")
            .concat(
                path.resolve("foo/x/main.eo").toString()
                .replace("\\", "\\\\")
            );
        final String res;
        if (error.equals("Warnings identified")) {
            res = str.concat(", \\d*: .*[\\s]*)+\\]");
        } else {
            res = str.concat(", \\d+: .*[\\s]*)+\\]");
        }
        return res;
    }
}
