import { afterEach, describe, expect, it, vi } from "vitest";
import { apiRequest } from "./client";

afterEach(() => vi.restoreAllMocks());

function response(body: unknown, init: ResponseInit = {}) {
  return new Response(JSON.stringify(body), {
    status: 200,
    headers: { "content-type": "application/json" },
    ...init
  });
}

describe("apiRequest", () => {
  it("sends JSON requests with credentials", async () => {
    const fetchMock = vi.spyOn(globalThis, "fetch").mockResolvedValue(response({
      success: true,
      code: "ok",
      message: "",
      data: { id: 1 }
    }));

    const result = await apiRequest<{ id: number }>("/api/v1/items", {
      method: "POST",
      body: JSON.stringify({ name: "test" })
    });

    expect(result.data.id).toBe(1);
    expect(fetchMock).toHaveBeenCalledWith("/api/v1/items", expect.objectContaining({
      credentials: "include",
      headers: expect.objectContaining({ "Content-Type": "application/json" })
    }));
  });

  it("normalizes non-JSON unauthorized responses", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(new Response("login", {
      status: 401,
      headers: { "content-type": "text/html" }
    }));
    await expect(apiRequest("/api/v1/private")).resolves.toMatchObject({
      success: false,
      code: "unauthorized"
    });
  });

  it("rejects non-JSON server responses", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(new Response("error", {
      status: 500,
      headers: { "content-type": "text/plain" }
    }));
    await expect(apiRequest("/api/v1/private")).resolves.toMatchObject({
      success: false,
      code: "non_json_response"
    });
  });

  it("does not accept a successful body with a failing HTTP status", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(response({
      success: true,
      code: "",
      message: "",
      data: {}
    }, { status: 500 }));
    await expect(apiRequest("/api/v1/items")).resolves.toMatchObject({
      success: false,
      code: "http_error"
    });
  });

  it("supports responses without JSON parsing", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(new Response(null, { status: 204 }));
    await expect(apiRequest("/download", { skipJsonParse: true })).resolves.toMatchObject({
      success: true,
      code: "ok"
    });
  });

  it("preserves API error codes and supports absolute form requests", async () => {
    const fetchMock = vi.spyOn(globalThis, "fetch").mockResolvedValue(response({
      success: false,
      code: "conflict",
      message: "duplicate",
      data: {}
    }, { status: 409 }));
    await expect(apiRequest("https://example.test/api", {
      method: "POST",
      body: new URLSearchParams({ id: "1" })
    })).resolves.toMatchObject({ code: "conflict", message: "duplicate" });
    expect(fetchMock).toHaveBeenCalledWith("https://example.test/api", expect.objectContaining({
      headers: expect.not.objectContaining({ "Content-Type": "application/json" })
    }));
  });
});
