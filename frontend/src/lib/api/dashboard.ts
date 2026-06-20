import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type DashboardSummary = {
  cards: {
    health: string;
    location: string;
    financeIncome: string;
    financeExpense: string;
  };
  locationTrend: {
    labels: string[];
    values: string[];
  };
  financeTrend: {
    labels: string[];
    income: string[];
    expense: string[];
  };
};

export function getDashboardSummary(): Promise<ApiResponse<DashboardSummary>> {
  return apiRequest<DashboardSummary>("/api/dashboard/summary");
}
