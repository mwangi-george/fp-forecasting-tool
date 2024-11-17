heading_style <-  "color: #2d677d; font-weight: bold; text-align: center;"


disconnection_notification_style <- HTML("
    #shiny-disconnected-overlay:before {
      content: 'Server closed due to idle session or an error. Please reload this page';
      font-size: 24px;
      color: #444;
      background-color: #fff;
      padding: 20px;
      border-radius: 8px;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      text-align: center;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
  ")
