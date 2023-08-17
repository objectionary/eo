
/**
 * Rust.
 *
 * @since 0.29
 * @checkstyle MethodNameCheck (100 lines)
 * @checkstyle LineLengthCheck (100 lines)
 * @checkstyle TypeNameCheck (5 lines)
 * @todo #2283:90min Create Universe class. Now its functionality is
 *  assigned to "EORust", which is why it is overcomplicated. "Universe"
 *  should perform a model of interaction with "eo" objects through
 *  methods "find", "put", "copy", "dataize" and "bind".
 */
@XmirObject(oname = "rust")
public class EOrust extends PhDefault {

    /**
     * Map with location of the `code` attribute as the key
     * and native method as the value.
     */
    private static final ConcurrentHashMap<String, String> NAMES;

    static {
        try {
            NAMES = load("target/names");
        } catch (final IOException exc) {
            throw new ExFailure(
                "Cannot read the file target/eo-test/names",
                exc
            );
        }
        final String lib;
        if (SystemUtils.IS_OS_WINDOWS) {
            lib = "common.dll";
        } else if (SystemUtils.IS_OS_LINUX) {
            lib = "libcommon.so";
        } else if (SystemUtils.IS_OS_MAC) {
            lib = "libcommon.dylib";
        } else {
            throw new NotImplementedException(
                String.format(
                    "Rust inserts are not supported by %s os. Only windows, linux and macos are allowed.",
                    System.getProperty("os.name")
                )
            );
        }
        final File libs = Paths.get("target")
            .resolve("eo-test")
            .resolve("Lib").toFile();
        if (libs.isDirectory()) {
            for (final File subdir: libs.listFiles()) {
                final Path path = subdir.toPath()
                    .resolve("target")
                    .resolve("debug")
                    .resolve(lib)
                    .toAbsolutePath();
                if (path.toFile().exists()) {
                    System.load(path.toString());
                }
            }
        }
    }

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOrust(final Phi sigma) {
        super(sigma);
        this.add("code", new AtFree());
        this.add("params", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String name = NAMES.get(
                        rho.attr("code").get().locator().split(":")[0]
                    );
                    final Method method = Class.forName(
                        String.format(
                            "EOrust.natives.%s",
                            name
                        )
                    ).getDeclaredMethod(name, Universe.class);
                    if (method.getReturnType() != byte[].class) {
                        throw new ExFailure(
                            "Return type of %s is %s, required %s",
                            method,
                            method.getReturnType(),
                            byte[].class
                        );
                    }
                    final Phi portal = new Dataized(
                        rho
                        .attr("params").get()
                        .attr("Δ").get()
                    ).take(Phi[].class)[0];
                    return EOrust.translate(
                        (byte[]) method.invoke(
                            null, new Universe(
                                portal
                            )
                        )
                    );
                }
            )
        );
    }

    /**
     * Loads names map.
     * @param src Where to load from.
     * @return Names map.
     * @throws IOException If any issues with IO.
     */
    private static ConcurrentHashMap<String, String> load(final String src) throws IOException {
        try (ObjectInputStream map = new ObjectInputStream(
            new ByteArrayInputStream(
                new IoCheckedBytes(
                    new Base64Bytes(
                        new BytesOf(
                            new TextOf(Paths.get(src))
                        )
                    )
                ).asBytes()
            )
        )) {
            final Object result = map.readObject();
            if (result.getClass() != ConcurrentHashMap.class) {
                throw new ClassCastException(
                    String.format(
                        "Object inside %s has wrong class %s",
                        src,
                        result.getClass()
                    )
                );
            }
            return (ConcurrentHashMap<String, String>) result;
        } catch (final ClassNotFoundException exc) {
            throw new IllegalArgumentException(
                String.format(
                    "File %s contains invalid data",
                    src
                ),
                exc
            );
        }
    }

    /**
     * Translates byte message from rust side to Phi object.
     * @param message Message that native method returns.
     * @return Phi object.
     * @todo #2283:45min Implement handling of vertex returning.
     *  It must convert message array from 1 to last byte to the int
     *  and return eo object with corresponding vertex then.
     * @todo #2283:45min Implement handling of String returning.
     *  It must convert message array from 1 to last byte to the String
     *  and return eo object with converted String Data.
     */
    private static Phi translate(final byte[] message) {
        final byte determinant = message[0];
        final byte[] content = Arrays.copyOfRange(message, 1, message.length);
        final Phi ret;
        switch (determinant) {
            case 0:
                throw new ExFailure(
                    "Returning vertex is not implemented yet"
                );
            case 1:
                ByteBuffer buffer = ByteBuffer.allocate(Double.BYTES);
                buffer.put(content);
                buffer.flip();
                ret = new Data.ToPhi(buffer.getDouble());
                break;
            case 2:
                buffer = ByteBuffer.allocate(Long.BYTES);
                buffer.put(content);
                buffer.flip();
                ret = new Data.ToPhi(buffer.getLong());
                break;
            default:
                throw new ExFailure(
                    "Returning Strings and raw bytes is not implemented yet"
                );
        }
        return ret;
    }
}
