/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOdirectory$EOdeleted$EOentries}.
 * @since 0.63
 */
final class EOdirectoryEOdeletedEOentriesTest {

    @Test
    void staysEmptyWhenTheRootIsGoneBeforeTheWalkStarts(@TempDir final Path temp)
        throws IOException {
        final Path root = temp.resolve("уже-нет");
        Files.createDirectory(root);
        Files.delete(root);
        MatcherAssert.assertThat(
            "entries of a root removed before the walk started is not empty",
            new TupleToArray(this.entries(root)).get().length,
            Matchers.equalTo(0)
        );
    }

    @Test
    void listsEveryDescendantWhenTheRootStays(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("файл"), new byte[0]);
        MatcherAssert.assertThat(
            "entries of a root that stayed in place does not list its child",
            new TupleToArray(this.entries(temp)).get().length,
            Matchers.equalTo(2)
        );
    }

    /**
     * Make an application of the atom to the directory at this path.
     * @param path The path of the directory to delete
     * @return The atom applied to the directory
     */
    private Phi entries(final Path path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path.toString()));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        return new PhApplication(
            new EOdirectory$EOdeleted$EOentries(), Phi.RHO, directory.take("deleted")
        );
    }
}
