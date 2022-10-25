package org.eolang.maven;

import java.nio.file.Path;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

public class DepgraphMavenPlugin implements DependenciesPlugin {

    /**
     * Maven project.
     */
    private final MavenProject project;

    /**
     * Maven session.
     */
    private final MavenSession session;

    /**
     * Maven plugin manager.
     */
    private final BuildPluginManager manager;

    private final Path dir;

    public DepgraphMavenPlugin(
        final MavenProject project,
        final MavenSession session,
        final BuildPluginManager manager,
        final Path dir
    ) {
        this.project = project;
        this.session = session;
        this.manager = manager;
        this.dir = dir;
    }

    @Override
    public Path dependenciesFile(final Dependency origin) {
        try {
            final String name = DepgraphMavenPlugin.fileName(origin);
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("com.github.ferstl"),
                    MojoExecutor.artifactId("depgraph-maven-plugin"),
                    MojoExecutor.version("4.0.2")
                ),
                MojoExecutor.goal("for-artifact"),
                MojoExecutor.configuration(
                    MojoExecutor.element("groupId", origin.getGroupId()),
                    MojoExecutor.element("artifactId", origin.getArtifactId()),
                    MojoExecutor.element("version", origin.getVersion()),
                    MojoExecutor.element("graphFormat", "json"),
                    MojoExecutor.element("outputDirectory", dir.toString()),
                    MojoExecutor.element("outputFileName", name)
                ),
                MojoExecutor.executionEnvironment(
                    this.project,
                    this.session,
                    this.manager
                )
            );
            return dir.resolve(name);
        } catch (MojoExecutionException e) {
            throw new IllegalStateException(e);
        }
    }

    private static String fileName(final Dependency dependency) {
        return String.format("%s_%s_%s%s",
            dependency.getGroupId(),
            dependency.getArtifactId(),
            dependency.getVersion(),
            ".json"
        );
    }
}
