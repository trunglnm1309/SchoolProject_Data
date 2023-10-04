import React, { useEffect } from "react";
import "./Error.css";
function ErrorPage() {
  return (
    <div className="page-error">
      <div className="container">
        <div className="row">
          <div className="col-12">
            <img
              src="https://drudesk.com/sites/default/files/2018-02/404-error-page-not-found.jpg"
              alt=""
              className="error404"
            />
          </div>
        </div>
      </div>
    </div>
  );
}

export default ErrorPage;
