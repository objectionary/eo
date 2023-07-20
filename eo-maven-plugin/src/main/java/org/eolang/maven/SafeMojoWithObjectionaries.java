package org.eolang.maven;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.objectionary.*;

/**
 * Abstract mojo with objectionaries.
 *
 * @since 0.29.6
 */
abstract class SafeMojoWithObjectionaries extends SafeMojo {
    /**
     * The Git hash to pull objects from, in objectionary.
     *
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    protected String tag = "master";

    /**
     * Read hashes from local file.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "offlineHashFile")
    protected Path offlineHashFile;

    /**
     * Return hash by pattern.
     * -DofflineHash=0.*.*:abc2sd3
     * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "offlineHash")
    protected String offlineHash;

    /**
     * The objectionary.
     */
    @SuppressWarnings("PMD.ImmutableField")
    protected Objectionary objectionary;

    /**
     * Hash-Objectionary map.
     * @todo #1602:30min Use objectionaries to pull objects with different
     *  versions. Objects with different versions are stored in different
     *  storages (objectionaries). Every objectionary hash its own hash.
     *  To pull versioned object from objectionary firstly we need to get
     *  right objectionary by object's version and then get object from that
     *  objectionary by name.
     * @todo #1602:30min Use objectionaries to probe objects with different
     *  versions. Objects with different versions are stored in different
     *  storages (objectionaries). Every objectionary has its own hash.
     *  To get versioned object from objectionary firstly we need to get
     *  right objectionary by object's version and then get object from that
     *  objectionary by name.
     * @checkstyle MemberNameCheck (5 lines)
     */
    protected final Map<String, Objectionary> objectionaries = new HashMap<>();

    /**
     * Get objectionary from map by given hash.
     * @param hash Hash.
     * @return Objectionary by given hash.
     */
    abstract protected Objectionary objectionaryBy(final CommitHash hash);
}
