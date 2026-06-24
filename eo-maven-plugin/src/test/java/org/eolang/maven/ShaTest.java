package org.eolang.maven;

import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;

/**
 * Tests for {@link Sha}.
 */
final class ShaTest {

    @Test
    void doesNotCollideOnDifferentFileBoundaries(@TempDir final Path tmp)
        throws Exception {
        // Directory A: file1="ab", file2="c"  → concat = "abc"
        final Path dirA = tmp.resolve("A");
        Files.createDirectory(dirA);
        Files.writeString(dirA.resolve("file1"), "ab");
        Files.writeString(dirA.resolve("file2"), "c");

        // Directory B: file1="a", file2="bc"  → concat = "abc"
        final Path dirB = tmp.resolve("B");
        Files.createDirectory(dirB);
        Files.writeString(dirB.resolve("file1"), "a");
        Files.writeString(dirB.resolve("file2"), "bc");

        assertThat(
            "directories with same content concatenation but different file boundaries must not collide",
            new Sha(dirA).toString(),
            not(equalTo(new Sha(dirB).toString()))
        );
    }

    @Test
    void doesNotCollideOnFileRename(@TempDir final Path tmp)
        throws Exception {
        // Same content, different file name
        final Path dirA = tmp.resolve("A");
        Files.createDirectory(dirA);
        Files.writeString(dirA.resolve("foo.txt"), "hello");

        final Path dirB = tmp.resolve("B");
        Files.createDirectory(dirB);
        Files.writeString(dirB.resolve("bar.txt"), "hello");

        assertThat(
            "directories differing only in file name must not collide",
            new Sha(dirA).toString(),
            not(equalTo(new Sha(dirB).toString()))
        );
    }

    @Test
    void sameDirectoryProducesSameHash(@TempDir final Path tmp)
        throws Exception {
        final Path dir = tmp.resolve("C");
        Files.createDirectory(dir);
        Files.writeString(dir.resolve("a.txt"), "consistent");
        assertThat(
            "same directory must always produce the same hash",
            new Sha(dir).toString(),
            equalTo(new Sha(dir).toString())
        );
    }
}