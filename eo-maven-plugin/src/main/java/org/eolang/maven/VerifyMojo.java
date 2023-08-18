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

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Mojo that checks errors and warnings after "assemble" phase.
 *
 * @since 0.31.0
 * @todo #1708:30min Implement VerifyMojo. VerifyMojo should check all errors
 *  and critical errors in xmir after {@link AssembleMojo} is finished. Also if
 *  {@code failOnWarning} flag is set to true - mojo should check warnings. When
 *  mojo is implemented - need to remove "failOnError" flag from
 *  {@link OptimizeMojo} and put "verify" step right after "assemble" in all
 *  pom.xml files
 */
@Mojo(
    name = "verify",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class VerifyMojo extends SafeMojo {
    /**
     * Whether we should fail on warning.
     *
     * @checkstyle MemberNameCheck (11 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    @Override
    void exec() throws IOException {
        throw new UnsupportedOperationException(
            String.format(
                "The VerifyMojo is not implemented yet, failOnWarning is %s",
                this.failOnWarning
            )
        );
    }
}
