#include "field.h"
#include <QString>
#include <stdlib.h>

Field::Field(int width, int height)
{
    w = width;
    h = height;
    c = 7;
    pField = new int[w * h];
    memset(pField, 0, w * h * sizeof(int));
}

Field::Field(Field *field)
{
    w = field->w;
    h = field->h;
    pField = new int[w * h];
    memcpy(pField, field->pField, w * h * sizeof(int));
}

Field::~Field()
{
    delete pField;
}

int Field::width()
{
    return w;
}

int Field::height()
{
    return h;
}

int Field::cell(int x, int y)
{
    return pField[x + y * w];
}

void Field::setCell(int x, int y, int value)
{
    pField[x + y * w] = value;
}

int Field::colors()
{
    return c;
}

bool Field::throwBalls(Field *pThrowMap, int nBalls, bool reset)
{
    Field clone(this);
    int i, m, n = w * h;
    quint16 *points = new quint16[n];
    bool res = true;

    if (reset)
        for (i = 0; i < n; i ++)
			pThrowMap->pField[i] = 0;

    while (nBalls--)
    {
        m = 0;
        for (i = 0; i < n; i++)
        {
            if (!clone.pField[i])
            {
                points[m] = i;
                m++;
            }
        }
        if (!m) { res = false; break; }
        i = points[qrand() % m];
		pThrowMap->pField[i] = clone.pField[i] = 1 + qrand() % c;
    }

    delete points;

    return res;
}

void Field::clear()
{
	memset(pField, 0, w * h * sizeof(int));
}

int Field::count()
{
	int n = 0;
	for (int x = 0; x < w; x++)
		for (int y = 0; y < h; y++)
			if (cell(x, y))
				n++;
	return n;
}

bool Field::hasPath(int x1, int y1, int x2, int y2, Field *searchResult)
{
    int step = searchResult->pField[x1 + w * y1] = -1;
    //int goal = searchResult->pField[x2 + w * y2] = -w * h;

    bool bContinue;
    int x, y;

    do
    {
        bContinue = false;
        for (y = 0; y < h; y++)
        {
            for (x = 0; x < w; x++)
            {
                if (searchResult->cell(x, y) == step)
                {
                    if (x == x2 && y == y2)
                        return true;

                    if (x > 0 && searchResult->cell(x - 1, y) == 0)
                    {
                        searchResult->setCell(x - 1, y, step - 1);
                        bContinue = true;
                    }

                    if (x < w - 1 && searchResult->cell(x + 1, y) == 0)
                    {
                         searchResult->setCell(x + 1, y, step - 1);
                        bContinue = true;
                    }

                    if (y > 0 && searchResult->cell(x, y - 1) == 0)
                    {
                        searchResult->setCell(x, y - 1, step - 1);
                        bContinue = true;
                    }

                    if (y < h - 1 && searchResult->cell(x, y + 1) == 0)
                    {
                        searchResult->setCell(x, y + 1, step - 1);
                        bContinue = true;
                    }
                }
            }
        }
        step--;
    } while (bContinue);

    return false;
}

void Field::shortestPath(int x, int y, Field *searchResult)
{
    int c;
    while ((c = cell(x, y)) < -1)
    {
#ifdef QT_DEBUG
        qDebug("Cur.Pos (%d, %d)", x, y);
#endif
        searchResult->setCell(x, y, c);
        if (x > 0 && cell(x - 1, y) == c + 1)
            x--;
        else if (y > 0 && cell(x, y - 1) == c + 1)
            y--;
		else if (x < w - 1 && cell(x + 1, y) == c + 1)
            x++;
		else if (y < h - 1 && cell(x, y + 1) == c + 1)
            y++;
    }
#ifdef QT_DEBUG
    qDebug("shortestPath ended.");
#endif
}

QLinkedList<QPoint> Field::waypoints(int x, int y)
{
    QLinkedList<QPoint> result;
    /*
      \ 3 /
      2 + 0
      / 1 \
      */
    int dir = -1;
    int c;
    while ((c = cell(x, y)) < -1)
    {
        if (x > 0 && cell(x - 1, y) == c + 1)
        {
            if (dir != 2)
            {
                dir = 2;
                result.prepend(QPoint(x, y));
            }
            x--;
        }
        else if (y > 0 && cell(x, y - 1) == c + 1)
        {
            if (dir != 3)
            {
                dir = 3;
                result.prepend(QPoint(x, y));
            }
            y--;
        }
		else if (x < w - 1 && cell(x + 1, y) == c + 1)
        {
            if (dir != 0)
            {
                dir = 0;
                result.prepend(QPoint(x, y));
            }
            x++;
        }
		else if (y < h - 1 && cell(x, y + 1) == c + 1)
        {
            if (dir != 1)
            {
                dir = 1;
                result.prepend(QPoint(x, y));
            }
            y++;
        }
    }

    result.prepend(QPoint(x, y));

    return result;
}

bool Field::deletionMap(Field *searchResult, GameType gameType)
{
    int n;
	bool res = false;
	searchResult->clear();
	for (int c, x = 0; x < w; x++)
		for (int y = 0; y < h; y++)
            if ((c = cell(x, y)))
			{
                switch (gameType)
                {
                case Lines:
                    for (int k = 0; k < 4; k++)
                        if ((n = lineLength(x, y, k)) >= 5)
                        {
                            searchResult->markLine(x, y, k, n);
                            res = true;
                        }
                    break;
                case Squares:
                    if (x < w - 1 && y < h - 1 && c == cell(x + 1, y) && c == cell(x, y + 1) && c == cell(x + 1, y + 1))
                    {
                        searchResult->setCell(x, y, 1);
                        searchResult->setCell(x + 1, y, 1);
                        searchResult->setCell(x, y + 1, 1);
                        searchResult->setCell(x + 1, y + 1, 1);
                        res = true;
                    }
                    break;
                case Blocks:
                    if (blockSize(x, y) >= 7)
                    {
                        searchResult->setCell(x, y, 1);
                        res = true;
                    }
                    break;
                }

			}
	return res;
}

void Field::operator += (Field inclusion)
{
	inclusion = inclusion;
}

int Field::lineLength(int x0, int y0, int dir)
{
	int dx = (dir < 2) ? 1 : (dir < 3) ? 0 : -1, dy = (dir > 0) ? 1 : 0;
	int x = x0, y = y0, n = 0;

	while (x >= 0 && x < w && y < h && cell(x, y) == cell(x0, y0))
	{
		x += dx;
		y += dy;
		n++;
	}
	return n;
}

void Field::markLine(int x0, int y0, int dir, int length)
{
	int dx = (dir < 2) ? 1 : (dir < 3) ? 0 : -1, dy = (dir > 0) ? 1 : 0;
	for (int x = x0, y = y0; length; x += dx, y += dy, length--)
		setCell(x, y, 1);
}

int Field::blockSize(int x0, int y0)
{
    int c = cell(x0, y0), res = 1, step = -1;
    Field search(w, h);
    search.setCell(x0, y0, -1);
    bool bContinue;
    do
    {
        bContinue = false;
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
                if (search.cell(x, y) == step)
                {
                    if (x > 0 && cell(x - 1, y) == c && search.cell(x - 1, y) == 0)
                    {
                        search.setCell(x - 1, y, step - 1);
                        bContinue = true;
                        res++;
                    }

                    if (x < w - 1 && cell(x + 1, y) == c && search.cell(x + 1, y) == 0)
                    {
                        search.setCell(x + 1, y, step - 1);
                        bContinue = true;
                        res++;
                    }

                    if (y > 0 && cell(x, y - 1) == c && search.cell(x, y - 1) == 0)
                    {
                        search.setCell(x, y - 1, step - 1);
                        bContinue = true;
                        res++;
                    }

                    if (y < h - 1 && cell(x, y + 1) == c && search.cell(x, y + 1) == 0)
                    {
                        search.setCell(x, y + 1, step - 1);
                        bContinue = true;
                        res++;
                    }
                }
        step--;
    } while (bContinue);
    return res;
}
