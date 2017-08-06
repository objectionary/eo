/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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

import java.io.File;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.io.InputOf;
import org.cactoos.io.LengthOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.eolang.compiler.CompileException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.rules.TemporaryFolder;

/**
 * Test case for {@link CompileMojo}.
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class CompileMojoTest extends AbstractMojoTestCase {

    /**
     * Temp dir.
     * @checkstyle VisibilityModifier (3 lines)
     */
    public final transient TemporaryFolder temp = new TemporaryFolder();

    /**
     * Main can print a simple text.
     * @throws Exception If some problem inside
     */
    public void testSimpleCompilation() throws Exception {
        final CompileMojo mojo = new CompileMojo();
        this.temp.create();
        final File src = this.temp.newFolder();
        this.setVariableValueToObject(mojo, "sourceDirectory", src);
        new LengthOf(
            new TeeInput(
                new InputOf(
                    "type Pixel:\n  Pixel moveTo(Integer x, Integer y)"
                ),
                new OutputTo(new File(src, "main.eo"))
            )
        ).value();
        final File target = this.temp.newFolder();
        this.setVariableValueToObject(mojo, "targetDirectory", target);
        this.setVariableValueToObject(mojo, "project", new MavenProjectStub());
        mojo.execute();
        MatcherAssert.assertThat(
            new File(target, "Pixel.java").exists(),
            Matchers.is(true)
        );
    }

    /**
     * Main can print a simple text.
     * @throws Exception If some problem inside
     */
    public void testCrashOnInvalidSyntax() throws Exception {
        final CompileMojo mojo = new CompileMojo();
        this.temp.create();
        final File src = this.temp.newFolder();
        this.setVariableValueToObject(mojo, "sourceDirectory", src);
        this.setVariableValueToObject(
            mojo, "targetDirectory", this.temp.newFolder()
        );
        new LengthOf(
            new TeeInput(
                new InputOf("something is wrong here"),
                new OutputTo(new File(src, "f.eo"))
            )
        ).value();
        try {
            mojo.execute();
            throw new IllegalArgumentException("exception is not here, why?");
        } catch (final CompileException ex) {
            assert ex != null;
        }
    }

}
