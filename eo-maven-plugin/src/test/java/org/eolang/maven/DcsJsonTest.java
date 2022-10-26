package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class DcsJsonTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, 3",
        "eo-math-dependencies-without-foreign.json, 7"
    })
    void readsAllDependenciesFromJsonFile(
        final String name,
        final int number,
        @TempDir final Path tmp
    ) {
        MatcherAssert.assertThat(
            new DcsDepgraph.DcsJson(file(tmp, name)).all().size(),
            Matchers.equalTo(number)
        );

    }

    private Path file(final Path tmp, final String name) {
        try {
            final Path res = tmp.resolve(name);
            new Home().save(
                new ResourceOf(
                    String.format("org/eolang/maven/dependencies/%s", name)
                ),
                res
            );
            return res;
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }
}
