package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.util.Collection;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

public class ForeignTojos implements Closeable {

    private final Unchecked<Tojos> tojos;

    public ForeignTojos(final Scalar<Tojos> scalar) {
        this.tojos = new Unchecked<>(new Sticky<>(scalar));
    }

    @Override
    public void close() throws IOException {
        this.tojos.value().close();
    }

    public Collection<Tojo> scoped(final String scope) {
        return this.tojos.value().select(
            row -> row.exists(Attribute.XMIR_2.key())
                && row.get(Attribute.SCOPE.key()).equals(scope)
        );
    }

    public Collection<Tojo> forEo(final String eo) {
        return this.tojos.value().select(
            row -> row.exists(Attribute.XMIR_2.key())
                && row.get(Attribute.EO.key()).equals(eo)
        );
    }

    public int size() {
        return this.tojos.value().select(all -> true).size();
    }

    public Tojos value() {
        return this.tojos.value();
    }

    enum Attribute {

        /**
         * Tojo id.
         */
        ID("id"),

        /**
         * Tojo eo file.
         */
        EO("eo"),

        /**
         * Version of eo.
         */
        VERSION("version"),

        /**
         * Tojo xmir file.
         */
        XMIR("xmir"),

        /**
         * Tojo xmir2 file.
         */
        XMIR_2("xmir2"),

        /**
         * Absolute location of SODG file.
         */
        SODG("sodg"),

        /**
         * Absolute location of JAR file.
         */
        JAR("jar"),

        /**
         * Discovered.
         */
        DISCOVERED("discovered"),

        /**
         * Where this object was discovered.
         */
        DISCOVERED_AT("discovered-at"),

        /**
         * Probed.
         */
        PROBED("probed"),

        /**
         * Scope.
         */
        SCOPE("scope"),

        /**
         * Transpiled.
         */
        TRANSPILED("transpiled"),
        HASH("hash");

        /**
         * Attribute name.
         */
        private final String key;

        /**
         * Ctor.
         * @param attribute The attribute name.
         */
        Attribute(final String attribute) {
            this.key = attribute;
        }

        /**
         * Get the attribute name.
         * @return The attribute name.
         */
        public String key() {
            return this.key;
        }
    }
}
