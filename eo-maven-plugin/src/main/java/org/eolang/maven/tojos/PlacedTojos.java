package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.util.FileHash;

/**
 * PlacedTojos encapsulates tojos logic and keeps short information about all placed files.
 *
 * @since 0.30
 */
public class PlacedTojos implements Closeable {

    /**
     * All tojos.
     */
    private final Unchecked<? extends Tojos> all;

    public PlacedTojos(final Sticky<? extends Tojos> tojos) {
        this(new Unchecked<>(tojos));
    }

    private PlacedTojos(final Unchecked<? extends Tojos> tojos) {
        this.all = tojos;
    }

    @Override
    public void close() throws IOException {
        this.all.value().close();
    }

    public Collection<Tojo> dependencyJar(final String dep) {
        return this.all.value().select(
            row -> row.get(Tojos.KEY).equals(dep)
                && "jar".equals(row.get(Attribute.KIND.key()))
        );
    }

    public Collection<Tojo> allClasses() {
        return this.all.value().select(row -> "class".equals(row.get(Attribute.KIND.key())));
    }

    public Collection<Tojo> allJars() {
        return this.all.value().select(row -> "jar".equals(row.get(Attribute.KIND.key())));
    }

    public void addDependency(final String dep) {
        this.all.value().add(dep).set(Attribute.KIND.key(), "jar");
    }

    public Tojo addClass(final Path target, final String related, final String dep) {
        final String id = target.toString();
        return this.all.value().add(id)
            .set(Attribute.KIND.key(), "class")
            .set(Attribute.HASH.key(), new FileHash(target))
            .set(Attribute.RELATED.key(), related)
            .set(Attribute.DEPENDENCY.key(), dep)
            .set(Attribute.UNPLACED.key(), "false");
    }

    public boolean isEmpty() {
        return this.all.value().select(row -> true).isEmpty();
    }

    /**
     * Check whether tojos was not unplaced.
     * @param before Tojos.
     * @return True if not unplaced.
     */
    public static boolean isNotUnplaced(final Iterable<? extends Tojo> before) {
        final Tojo tojo = before.iterator().next();
        return tojo.exists(Attribute.UNPLACED.key())
            && "false".equals(tojo.get(Attribute.UNPLACED.key()));
    }


    private enum Attribute {

        ID("id"),
        KIND("kind"),
        DEPENDENCY("dependency"),
        HASH("hash"),
        RELATED("related"),
        UNPLACED("unplaced"),
        ;

        private final String key;

        Attribute(final String key) {
            this.key = key;
        }

        String key() {
            return this.key;
        }
    }
}
