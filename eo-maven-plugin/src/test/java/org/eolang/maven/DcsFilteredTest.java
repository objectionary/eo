package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import org.apache.maven.model.Dependency;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;

class DcsFilteredTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, 1",
        "eo-math-dependencies-without-foreign.json, 0"
    })
    void findsAllTransitiveDependencies(
        final String name,
        final int number,
        @TempDir final Path tmp
    ) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId("org.eolang");
        dependency.setArtifactId("eo-math");
        dependency.setVersion("0.2.3");
        MatcherAssert.assertThat(
            new DcsFiltered(
                new DcsDepgraph.DcsJson(file(tmp, name)),
                Arrays.asList(
                    new DcsFiltered.NotRuntime(),
                    new DcsFiltered.NotSame(dependency),
                    new DcsFiltered.NotTesting()
                )
            ).all().size(),
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