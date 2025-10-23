#!/bin/bash
echo "=== 5-Year Data Fetch Progress ==="
tail -5 fetch_5years.log
echo ""
if [ -f "metaculus_calibration_data_5years.rds" ]; then
    echo "✓ Data fetch COMPLETE!"
    ls -lh metaculus_calibration_data_5years.*
else
    echo "Still processing... (check fetch_5years.log for details)"
fi
