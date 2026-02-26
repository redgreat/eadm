#!/bin/bash
# API Location Endpoint Test Script
# 测试 /api/location/track 端点

BASE_URL="http://localhost:8080"
API_ENDPOINT="${BASE_URL}/api/location/track"

echo "=========================================="
echo "API Location Endpoint Test"
echo "=========================================="
echo ""

# Test 1: Missing Authorization header (should return 401)
echo "Test 1: Missing Authorization header"
echo "Expected: 401 Unauthorized"
curl -s -w "\nHTTP Status: %{http_code}\n" \
  "${API_ENDPOINT}?device_type=garmin&start_time=2024-01-01T00:00:00Z&end_time=2024-01-01T23:59:59Z"
echo ""
echo "----------------------------------------"
echo ""

# Test 2: Invalid token (should return 401)
echo "Test 2: Invalid Authorization token"
echo "Expected: 401 Unauthorized"
curl -s -w "\nHTTP Status: %{http_code}\n" \
  -H "Authorization: Bearer invalid_token_here" \
  "${API_ENDPOINT}?device_type=garmin&start_time=2024-01-01T00:00:00Z&end_time=2024-01-01T23:59:59Z"
echo ""
echo "----------------------------------------"
echo ""

# Test 3: Missing required parameters (should return 400)
echo "Test 3: Missing required parameters"
echo "Expected: 400 Bad Request"
curl -s -w "\nHTTP Status: %{http_code}\n" \
  -H "Authorization: Bearer YOUR_VALID_TOKEN_HERE" \
  "${API_ENDPOINT}"
echo ""
echo "----------------------------------------"
echo ""

# Test 4: Invalid device type (should return 400)
echo "Test 4: Invalid device type"
echo "Expected: 400 Bad Request"
curl -s -w "\nHTTP Status: %{http_code}\n" \
  -H "Authorization: Bearer YOUR_VALID_TOKEN_HERE" \
  "${API_ENDPOINT}?device_type=invalid&start_time=2024-01-01T00:00:00Z&end_time=2024-01-01T23:59:59Z"
echo ""
echo "----------------------------------------"
echo ""

# Test 5: Valid request with valid token (should return 200)
echo "Test 5: Valid request with valid token"
echo "Expected: 200 OK with track data"
echo "Note: Replace YOUR_VALID_TOKEN_HERE with a real JWT token"
curl -s -w "\nHTTP Status: %{http_code}\n" \
  -H "Authorization: Bearer YOUR_VALID_TOKEN_HERE" \
  "${API_ENDPOINT}?device_type=garmin&start_time=2024-01-01T00:00:00Z&end_time=2024-01-01T23:59:59Z"
echo ""
echo "----------------------------------------"
echo ""

echo "=========================================="
echo "Test completed"
echo "=========================================="
echo ""
echo "To get a valid JWT token, first login:"
echo "curl -X POST ${BASE_URL}/api/v1/login \\"
echo "  -H 'Content-Type: application/json' \\"
echo "  -d '{\"username\":\"your_username\",\"password\":\"your_password\"}'"
echo ""
