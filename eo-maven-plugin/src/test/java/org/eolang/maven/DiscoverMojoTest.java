/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import com.yegor256.tojos.MnJson;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.Input;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Test case for {@link DiscoverMojo}.
 *
 * @since 0.28.11
 */
final class DiscoverMojoTest {

    /**
     * The name of foreign dependencies file.
     */
    private static final String EO_FOREIGN = "eo-foreign.json";

    @ParameterizedTest
    @CsvSource({
        "org/eolang/maven/mess.eo, 7",
        "org/eolang/maven/sum.eo, 0",
        "org/eolang/maven/withwarning.eo, 1"
    })
    void executeDiscoveryPhaseForEoCorrectPrograms(
        final String program,
        final int dependencies,
        final @TempDir Path tmp
    ) throws IOException {
        this.saveProgram(tmp, new ResourceOf(program));
        this.discover(tmp);
        final Deque<Map<String, String>> json = this.discoveredJsonEntries(tmp);
        final Map<String, String> first = json.removeFirst();
        MatcherAssert.assertThat(
            String.valueOf(dependencies),
            Matchers.equalTo(first.get("discovered"))
        );
        MatcherAssert.assertThat(dependencies, Matchers.equalTo(json.size()));
    }

    /*
     * @todo #1360:30min Make logger inside DiscoveryMojo non-static.
     *  Using logger as a field we will able to test and check different
     *  test-cases like
     *  - Discovery phase without program inside DiscoveryMojo
     *  - Discovery phase with already checked programs inside DiscoveryMojo
     */
    @Test
    @Disabled
    void executeDiscoveryPhaseWithoutProgram() {
        // todo: add logic after logger will become non-static
        Assertions.fail();
    }

    @Test
    @Disabled
    void executeDiscoveryPhaseWithAlreadyCheckedPrograms() {
        // todo: add logic after logger will become non-static
        Assertions.fail();
    }

    private Deque<Map<String, String>> discoveredJsonEntries(final Path tmp) {
        return new LinkedList<>(
            new MnJson(
                tmp.resolve(DiscoverMojoTest.EO_FOREIGN)
            ).read()
        );
    }

    private void saveProgram(final Path temp, final Input code) throws IOException {
        final Path program = temp.resolve("program.eo");
        new Home().save(code, program);
        final Path foreign = temp.resolve(DiscoverMojoTest.EO_FOREIGN);
        Catalogs.INSTANCE.make(foreign, "json")
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, program.toString());
    }

    private void discover(final Path temp) {
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve(DiscoverMojoTest.EO_FOREIGN);
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        new Moja<>(DiscoverMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
    }
}
