package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.Catalogs;
import org.eolang.maven.PlaceMojo;
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

    public PlacedTojos(final Path file) {
        this(Catalogs.INSTANCE.make(file));
    }

    public PlacedTojos(final Tojos tojos) {
        this(new Sticky<>(() -> tojos));
    }

    public PlacedTojos(final Supplier<? extends Tojos> tojos) {
        this(new Sticky<>(tojos::get));
    }

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

    public Collection<Tojo> findJar(final String dep) {
        return this.all.value().select(
            row -> row.get(Tojos.KEY).equals(dep)
                && "jar".equals(row.get(Attribute.KIND.key()))
        );
    }

    public Collection<PlacedTojo> classes() {
        return this.all.value()
            .select(row -> "class".equals(row.get(Attribute.KIND.key())))
            .stream().map(PlacedTojo::new).collect(Collectors.toList());
    }

    public Collection<PlacedTojo> jars() {
        return this.all.value().select(row -> "jar".equals(row.get(Attribute.KIND.key())))
            .stream().map(PlacedTojo::new).collect(Collectors.toList());
    }

    public void addDependency(final String dep) {
        this.all.value().add(dep)
            .set(Attribute.KIND.key(), "jar");
    }

    public PlacedTojo placeClass(final Path target, final String related, final String dep) {
        final String id = target.toString();
        return new PlacedTojo(
            this.all.value().add(id)
                .set(Attribute.KIND.key(), "class")
                .set(Attribute.HASH.key(), new FileHash(target))
                .set(Attribute.RELATED.key(), related)
                .set(Attribute.DEPENDENCY.key(), dep)
                .set(Attribute.UNPLACED.key(), "false")
        );
    }

    public void placeJar(final String name) {
        this.all.value().add(name)
            .set(Attribute.KIND.key(), "jar")
            .set(Attribute.DEPENDENCY.key(), String.format("%s.jar", name))
            .set(Attribute.UNPLACED.key(), "false");
    }

    public boolean isEmpty() {
        return this.all.value().select(row -> true).isEmpty();
    }

    public void unplaceAll() {
        this.all.value().select(placed -> true).forEach(
            tojo -> tojo.set(Attribute.UNPLACED.key(), "true")
        );
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

    public List<PlacedTojo> all() {
        return this.all.value().select(tojos -> true)
            .stream()
            .map(PlacedTojo::new)
            .collect(Collectors.toList());
    }

    enum Attribute {

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
