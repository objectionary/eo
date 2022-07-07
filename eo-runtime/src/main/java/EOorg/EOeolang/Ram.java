package EOorg.EOeolang;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * Random access.
 *
 * @since 0.19
 */
public enum Ram {
    /**
     * Ram instance.
     */
    INSTANCE;

    /**
     * Phi to File mapping.
     */
    private final Map<Phi, RandomAccessFile> addresses = new HashMap<>();

    /**
     * Read.
     * @param object Owner.
     * @param position Position.
     * @param length Length.
     * @return Byte array.
     * @throws IOException If fails.
     */
    public byte[] read(
        final Phi object,
        final int position,
        final int length
    ) throws IOException {
        final RandomAccessFile ram = init(object);
        ram.seek(position);
        final byte[] buffer = new byte[length];
        ram.readFully(buffer, 0, length);
        return buffer;
    }

    /**
     * Initialize storage.
     * @param phi Owner.
     * @return Storage file
     */
    private RandomAccessFile init(final Phi phi) {
        final long size = new Dataized(phi.attr("size").get()).take(Long.class);
        return this.addresses.computeIfAbsent(
            phi,
            o -> {
                try {
                    final RandomAccessFile file = new RandomAccessFile(
                        Files.createTempFile(
                            ManagementFactory
                                .getRuntimeMXBean()
                                .getName(),
                            ".mem"
                        ).toFile(),
                        "rw"
                    );
                    file.setLength(size);
                    return file;
                } catch (final IOException e) {
                    throw new UncheckedIOException(e);
                }
            }
        );
    }


    /**
     * Write.
     * @param object Owner.
     * @param position Position to write.
     * @param bytes Bytes to wite.
     * @throws IOException If fails.
     */
    public void write(
        final Phi object,
        final int position,
        final byte[] bytes
    ) throws IOException {
        final RandomAccessFile buffer = this.init(object);
        buffer.seek(position);
        buffer.write(bytes);
    }

}
