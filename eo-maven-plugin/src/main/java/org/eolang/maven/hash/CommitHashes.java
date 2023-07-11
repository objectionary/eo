package org.eolang.maven.hash;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.URL;
import org.cactoos.iterable.Mapped;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapEnvelope;
import org.cactoos.map.MapOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Commit hashes hash-table.
 * The keys - tags.
 * The values - compound hashes (7 chars)
 *
 * @since 0.29.5
 */
final public class CommitHashes extends MapEnvelope<String, String> {

    /**
     * Cached text of hashes.
     */
    private static final String CACHE = CommitHashes.safeLoaded();

    /**
     * The URL where the list is kept.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Ctor.
     */
    public CommitHashes() {
        super(
            new MapOf<>(
                new Mapped<>(
                    line -> {
                        final String[] split = line.asString().split("\\s+");
                        return new MapEntry<>(
                            split[1],
                            new ChNarrow(
                                new CommitHash.ChConstant(split[0])
                            ).value()
                        );
                    },
                    new Split(CommitHashes.CACHE, "\n")
                )
            )
        );
    }

    /**
     * Load all hashes and tags from given url only once.
     * @return Text with tags and hashes.
     */
    private static String safeLoaded() {
        String cache;
        try {
            cache = new UncheckedText(
                new TextOf(new URL(CommitHashes.HOME))
            ).asString();
        } catch (final IOException ex) {
            Logger.warn(
                CommitHashes.class,
                "Failed to load catalog of Git hashes from %s, because of %s: '%s'",
                CommitHashes.HOME, ex.getClass().getSimpleName(), ex.getMessage()
            );
            cache = "";
        }
        return cache;
    }
}
