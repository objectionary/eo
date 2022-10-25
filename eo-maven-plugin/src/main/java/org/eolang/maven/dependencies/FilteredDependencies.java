package org.eolang.maven.dependencies;

import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;

public class FilteredDependencies implements Dependencies {

    private final Dependencies dependencies;
    private final Collection<Predicate<Dependency>> filters;

    public FilteredDependencies(
        final Dependencies dependencies,
        final Collection<Predicate<Dependency>> filters
    ) {
        this.dependencies = dependencies;
        this.filters = filters;
    }

    @Override
    public List<Dependency> toList() {
        return dependencies.toList()
            .stream()
            .filter(this::applyAllFilters)
            .collect(Collectors.toList());
    }

    private boolean applyAllFilters(final Dependency dependency) {
        return filters.stream().allMatch(f -> f.test(dependency));
    }
}
