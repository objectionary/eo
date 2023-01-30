package org.eolang.maven.objectionary;

import java.net.URL;
import java.util.Set;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.cactoos.set.SetOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

final class ObjectsIndex {

    private final Sticky<? extends Set<String>> objects;

    ObjectsIndex() {
        this("https://home.objectionary.com/objectionary.lst");
    }

    private ObjectsIndex(final String address) {
        this(new Sticky<>(ObjectsIndex.loadIndex(address)));
    }

    private ObjectsIndex(final Sticky<? extends Set<String>> all) {
        this.objects = all;
    }

    public boolean contains(final String name) {
        return new Unchecked<>(this.objects).value().contains(name);
    }

    private static Scalar<Set<String>> loadIndex(final String address) {
        return new ScalarOf<>(
            () -> new SetOf<>(
                new Mapped<>(
                    ObjectsIndex::convert,
                    new Mapped<>(
                        Text::asString,
                        new Split(new TextOf(new InputOf(new URL(address))), System.lineSeparator())
                    )
                )
            )
        );
    }

    private static String convert(final String name) {
        return name.substring(0, name.length() - 3)
            .replace('/', '.')
            .substring(name.indexOf('/') + 1);
    }
}
