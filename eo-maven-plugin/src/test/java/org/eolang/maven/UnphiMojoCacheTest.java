/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.FpDefault;
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
    void usesCache(@Mktmp final Path temp) throws Exception {
        final Home workspace = new HmBase(temp);
        final Path phi = Paths.get("target/eo/phi/std.phi");
        final Path xmir = Paths.get("target/eo/1-parse/std.xmir");
        workspace.save(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            phi
        );
        final String cache = "some valid phi cache";
        new FpDefault(
            src -> cache,
            temp.resolve("cache").resolve(UnphiMojo.CACHE),
            "version-1.0",
            "123ZaRiFcHiK321",
            Path.of("std.xmir")
        ).apply(temp.resolve(phi), temp.resolve(xmir));
        this.executePhiToXmir(new Farea(temp));
        MatcherAssert.assertThat(
            "XMIR file recreated twice",
            new TextOf(workspace.load(xmir)),
            Matchers.equalTo(cache)
        );
    }

    @Test
    @ExpectedToFail
    void invalidatesCache(@Mktmp final Path temp) throws Exception {
        final Home workspace = new HmBase(temp);
        final Path phi = Paths.get("target/eo/phi/std.phi");
        final Path xmir = Paths.get("target/eo/1-parse/std.xmir");
        final Path cache = Path.of("cache")
            .resolve(UnphiMojo.CACHE)
            .resolve("version-1.0")
            .resolve("123ZaRiFcHiK321")
            .resolve("std.xmir");
        workspace.save(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            phi
        );
        new FpDefault(
            src -> "some valid phi cache",
            temp.resolve("cache").resolve(UnphiMojo.CACHE),
            "version-1.0",
            "123ZaRiFcHiK321",
            Path.of("std.xmir")
        ).apply(temp.resolve(phi), temp.resolve(xmir));
        Files.setLastModifiedTime(
            temp.resolve(phi),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        final File cached = temp.resolve(cache).toFile();
        final long old = cached.lastModified();
        this.executePhiToXmir(new Farea(temp));
        MatcherAssert.assertThat(
            "phi-to-xmir cache not invalidated",
            old,
            Matchers.lessThan(cached.lastModified())
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
