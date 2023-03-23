package org.eolang.maven.dependencies;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.maven.model.Dependency;

public final class DcsFake implements Iterable<Dependency> {

    private final Collection<Dependency> dependencies;

    DcsFake() {
        this(
            DcsFake.randDep(),
            DcsFake.randDep(),
            DcsFake.randDep()
        );
    }

    DcsFake(final int size) {
        this(Stream.generate(DcsFake::randDep).limit(size).collect(Collectors.toList()));
    }

    DcsFake(final Dependency... deps) {
        this(Arrays.asList(deps));
    }

    private DcsFake(final Collection<Dependency> deps) {
        this.dependencies = deps;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return this.dependencies.iterator();
    }

    static Dependency randDep(final String scope) {
        final Random rand = new SecureRandom();
        return DcsFake.dep(
            UUID.randomUUID().toString(),
            UUID.randomUUID().toString(),
            String.valueOf(Math.abs(rand.nextInt())),
            scope
        );
    }

    static Dependency randDep() {
        final Random rand = new SecureRandom();
        return DcsFake.dep(
            UUID.randomUUID().toString(),
            UUID.randomUUID().toString(),
            String.valueOf(Math.abs(rand.nextInt())),
            new String[]{"test", "compiled", "runtime"}[rand.nextInt(3)]
        );
    }

    static Dependency runtime() {
        return DcsFake.dep(
            "org.eolang",
            "eo-runtime",
            "0.30.0",
            "compiled"
        );
    }

    static Dependency dep(
        final String group,
        final String artifact,
        final String version,
        final String scope
    ) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId(group);
        dependency.setArtifactId(artifact);
        dependency.setVersion(version);
        dependency.setScope(scope);
        return dependency;
    }
}
