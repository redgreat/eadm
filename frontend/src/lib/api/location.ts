import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type LocationPoint = {
  utcTime: string;
  deviceNo: string;
  lng: string | number;
  lat: string | number;
};

export type LocationList = {
  items: LocationPoint[];
  total: number;
};

export function getLocationPoints(params: {
  deviceNo: string;
  startTime: string;
  endTime: string;
}): Promise<ApiResponse<LocationList>> {
  const query = new URLSearchParams();
  if (params.deviceNo.trim()) {
    query.set("deviceNo", params.deviceNo.trim());
  }
  query.set("startTime", params.startTime);
  query.set("endTime", params.endTime);
  return apiRequest<LocationList>(`/api/location?${query.toString()}`);
}
