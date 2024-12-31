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

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.farea.Farea;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junitpioneer.jupiter.ExpectedToFail;

/**
 * Test cases for caching in {@link UnphiMojo}.
 * @since 0.50.0
 */
@ExtendWith(MktmpResolver.class)
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class UnphiMojoCacheTest {

    @Test
    @ExpectedToFail
    void touchesCacheNot(@Mktmp final Path temp) throws Exception {
        final Home workspace = new HmBase(temp);
        workspace.save(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            Paths.get("target/eo/phi/std.phi")
        );
        workspace.save(
            "some valid XMIR after unphi",
            Paths.get("target/eo/1-parse/std.xmir")
        );
        final File xmir = temp.resolve("target/eo/1-parse/std.xmir").toFile();
        final long modified = xmir.lastModified();
        this.executePhiToXmir(new Farea(temp));
        MatcherAssert.assertThat(
            "XMIR file recreated twice",
            modified,
            Matchers.equalTo(xmir.lastModified())
        );
    }

    @Test
    void touchesInvalidCache(@Mktmp final Path temp) throws Exception {
        final Home workspace = new HmBase(temp);
        workspace.save(
            "some valid XMIR that appeared before phi",
            Paths.get("target/eo/1-parse/std.xmir")
        );
        workspace.save(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            Paths.get("target/eo/phi/std.phi")
        );
        final File xmir = temp.resolve("target/eo/1-parse/std.xmir").toFile();
        final long modified = xmir.lastModified();
        this.executePhiToXmir(new Farea(temp));
        MatcherAssert.assertThat(
            "phi-to-xmir cache not invalidated",
            modified,
            Matchers.lessThan(xmir.lastModified())
        );
    }

    private void executePhiToXmir(final Farea farea) throws IOException {
        farea.together(
            f -> {
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("phi-to-xmir");
                f.exec("compile");
            }
        );
    }
}
