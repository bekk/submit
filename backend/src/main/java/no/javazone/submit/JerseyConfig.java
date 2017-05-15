package no.javazone.submit;

import no.javazone.submit.api.filters.AuthenticatedWithAuth0Filter;
import no.javazone.submit.api.filters.AuthenticatedWithTokenFilter;
import no.javazone.submit.api.resources.RootResource;
import no.javazone.submit.api.resources.SubmissionResource;
import no.javazone.submit.api.resources.UserResource;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.springframework.stereotype.Component;

@Component
public class JerseyConfig extends ResourceConfig {

    public JerseyConfig() {
        register(MultiPartFeature.class);

        register(RootResource.class);
        register(SubmissionResource.class);
        register(UserResource.class);

        register(AuthenticatedWithTokenFilter.class);
	register(AuthenticatedWithAuth0Filter.class);
    }

}
