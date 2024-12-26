function App() {
  return (
    <>
      <a href="https://example.com">
        <img/>
      </a>
      <div>
      </div>
    </>
  );
};

const Translations = () => {
  const { t } = useTranslation("coi-platform-translations", {
    keyPrefix: keyPrefixes.main,
  });
  return (
    <>
      <Alert severity="info" icon={false}>
        <AlertTitle>{t("info_title")}</AlertTitle>
        <Typography variant="body2">{t("info_description")}</Typography>
      </Alert>
      <EditForm />
    </>
  );
};
