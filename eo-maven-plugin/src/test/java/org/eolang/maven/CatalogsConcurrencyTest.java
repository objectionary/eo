package org.eolang.maven;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.nio.file.Path;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.number.SumOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Concurrency tests for {@link Catalogs}.
 * All tests in that class must be executed in parallel and in order to be sure that
 * everything works fine it's important to run the tests many times.
 * @since 0.29.0
 * @todo #1574:30min MonoTojo is not thread safe.
 *  It's not possible to use it in parallel. It should be fixed.
 *  After that, remove the @Disabled annotation from the test below.
 */
class CatalogsConcurrencyTest {

    final static int CORES = Runtime.getRuntime().availableProcessors();

    @Disabled
    @Test
    void readsFromTojosConcurrently(@TempDir final Path tmp) {
        final Tojos tojos = Catalogs.INSTANCE.make(tmp.resolve("foreign"), "json");
        MatcherAssert.assertThat(
            new SumOf(
                new Threads<>(
                    CORES,
                    IntStream.range(0, CORES)
                        .mapToObj(i -> tojos.add(UUID.randomUUID().toString()))
                        .map(CatalogsConcurrencyTest::task)
                        .collect(Collectors.toList())
                )
            ),
            Matchers.equalTo(CORES)
        );
    }

    private static Scalar<Integer> task(final Tojo tojo) {
        return () -> {
            final String uuid = "uuid";
            tojo.set(uuid, UUID.randomUUID().toString());
            tojo.get(uuid);
            tojo.get(uuid);
            tojo.get(uuid);
            tojo.get(uuid);
            return 1;
        };
    }


}