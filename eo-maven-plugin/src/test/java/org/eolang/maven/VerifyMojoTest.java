/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link VerifyMojo}.
 *
 * @since 0.31.0
 */
class VerifyMojoTest {

    @Test
    @Disabled
    void doesNotFailWithNoErrorsAndWarnings(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Verify()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    @Disabled
    void detectsErrorsSuccessfully(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[] > main",
                    "  QQ.io.stdout",
                    "    \"Hello world\""
                )
                .execute(new FakeMaven.Verify()),
            "Program with noname attributes should have failed or error, but it didn't"
        );
    }

    @Test
    @Disabled
    void detectsWarningWithCorrespondingFlag(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[] > main",
                    "  [] > @",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", true)
                .execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should have failed on warning, but it didn't"
        );
    }

    @Test
    @Disabled
    void doesNotDetectWarningWithoutCorrespondingFlag(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[] > main",
                    "  [] > @",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", false)
                .execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should not have failed on warning without flag, but it does"
        );
    }

    @Test
    @Disabled
    void detectsCriticalError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[] > main",
                    "  [] > name",
                    "  [] > name"
                )
                .execute(new FakeMaven.Verify()),
            "Program with duplicate names should have failed on critical error, but it didn't"
        );
    }
}
