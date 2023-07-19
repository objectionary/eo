package org.eolang.maven;

import org.eolang.maven.objectionary.Objectionary;

/**
 * With "Hash-Objectionary" map.
 */
public interface WithObjectionaries {
    /**
     * Get objectionary by given hash.
     * @param hash Hash as string.
     * @return Objectionary by given hash.
     */
    Objectionary objectionaryBy(final String hash);
}
