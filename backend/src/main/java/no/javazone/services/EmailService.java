package no.javazone.services;

import no.javazone.SubmitConfiguration;
import org.apache.commons.mail.DefaultAuthenticator;
import org.apache.commons.mail.Email;
import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.SimpleEmail;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EmailService {

	private static final Logger log = LoggerFactory.getLogger(EmailService.class);

	private SubmitConfiguration configuration;

	public EmailService(SubmitConfiguration configuration) {
		this.configuration = configuration;
	}

	public void sendTokenToUser(String emailAddress, String token) {
		String emailBody = generateEmailBody(token);
		sendEmail(emailAddress, emailBody);
	}

	private void sendEmail(String emailAddress, String emailBody) {
		try {
			Email email = new SimpleEmail();
			email.setHostName("smtp.googlemail.com");
			email.setSmtpPort(465);
			email.setAuthenticator(new DefaultAuthenticator(configuration.smtpUser, configuration.smtpPass));
			email.setSSLOnConnect(true);
			email.setFrom("program@java.no");
			email.setSubject("JavaZone submission login");
			email.setMsg(emailBody);
			email.addTo(emailAddress);
			email.send();
			log.info("Token email was sent to: " + emailAddress);
		} catch (EmailException e) {
			log.warn("Couldn't send email to " + emailAddress, e);
		}
	}

	private String generateEmailBody(String token) {
		StringBuilder b = new StringBuilder();
		b.append("Ready to submit a talk to JavaZone, or editing your talk?\n\n");
		b.append("Use this link to log your browser in to our submitting system:\n");
		b.append(configuration.tokenLinkPrefix).append(token).append("\n\n");
		b.append("Don't know why you received this email? Someone probably just misspelled their email address. Don't worry, they can't do anything on your behalf without this link");
		return b.toString();
	}
}