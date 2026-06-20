import { render } from "solid-js/web";
import { Route, Router } from "@solidjs/router";
import App from "./app/App";
import CrontabsPage from "./routes/CrontabsPage";
import DashboardPage from "./routes/DashboardPage";
import DevicesPage from "./routes/DevicesPage";
import FinancePage from "./routes/FinancePage";
import HealthPage from "./routes/HealthPage";
import LocationPage from "./routes/LocationPage";
import LoginPage from "./routes/LoginPage";
import NotFoundPage from "./routes/NotFoundPage";
import RolesPage from "./routes/RolesPage";
import SystemInfoPage from "./routes/SystemInfoPage";
import UsersPage from "./routes/UsersPage";
import "./styles/app.css";

const root = document.getElementById("root");

if (!root) {
  throw new Error("Root element #root was not found.");
}

render(
  () => (
    <Router root={App}>
      <Route path="/" component={DashboardPage} />
      <Route path="/login" component={LoginPage} />
      <Route path="/health" component={HealthPage} />
      <Route path="/location" component={LocationPage} />
      <Route path="/finance" component={FinancePage} />
      <Route path="/crontab" component={CrontabsPage} />
      <Route path="/user" component={UsersPage} />
      <Route path="/role" component={RolesPage} />
      <Route path="/device" component={DevicesPage} />
      <Route path="/sysinfo" component={SystemInfoPage} />
      <Route path="*404" component={NotFoundPage} />
    </Router>
  ),
  root
);
