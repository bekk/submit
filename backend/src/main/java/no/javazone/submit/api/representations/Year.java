package no.javazone.submit.api.representations;

import java.util.List;

public class Year {

    public String year;
    public String slug;
    public List<Submission> submissions;

    public Year(String year, String slug, List<Submission> submissions) {
        this.year = year;
        this.slug = slug;
        this.submissions = submissions;
    }

    public void addSubmission(Submission submission) {
        submissions.add(submission);
    }
}
