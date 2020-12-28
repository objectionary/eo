import org.eolang.*;
public final class Main {
    public static void main(final String... args) throws Exception {
        final Phi[] list = new Phi[args.length];
        for (int idx = 0; idx < args.length; ++idx) {
            final Phi phi = new EOint();
            final long num = Long.parseLong(args[idx]);
            phi.put("eo_self", () -> new Data.Value<>(num));
            list[idx] = phi;
        }
        final Phi app = new EOmain();
        app.put("args", () -> new Data.Value<>(list));
        if (!new Data.Take(app).take(Boolean.class)) {
            throw new RuntimeException();
        }
    }

}
