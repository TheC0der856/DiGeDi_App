UI_styles <- function() {
  tags$style(HTML('
      .header {
        display: flex;
        align-items: center;
        padding-top: 2rem;
        justify-content: space-between;
        align-items: center;
      }
      .header-with-image {
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .logo {
        width: auto; /* Größe des Logos anpassen */
        height: 25px;
        margin-right: 5px; /* Abstand zwischen Logo und Titel */
      }
      .title {
        font-size: 24px; /* Größe des Titels anpassen */
      }
      .btn-with-image {
        display: flex;
        align-items: center;
        padding: 3px;
      }
      .btn-with-image img {
        margin-left: 3px; /* Bild rechts vom Text */
      }
      .btn {
        white-space: normal !important;
        text-align: left;
      }
      .info-icon {
        font-size: 16px;
        color: inherit; /* Keine blaue Farbe, übernimmt Standardtextfarbe */
        cursor: pointer;
        margin-left: 5px;
      }
    '))
}