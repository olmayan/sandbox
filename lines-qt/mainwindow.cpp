#include "mainwindow.h"
#include <QPainter>
#include <QTime>
#include <QPropertyAnimation>
#include <QMenuBar>
#include <QApplication>

#define CELL_SIDE 40

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    qsrand(QTime::currentTime().msec());
    pGameField = new Field(W, H);
    pNextBalls = new Field(W, H);
	pDeletionMap = new Field(W, H);

    graphicsScene = new QGraphicsScene(0, 0, CELL_SIDE * W, CELL_SIDE * H);
    graphicsView = new GraphicsView(graphicsScene);
    this->setCentralWidget(graphicsView);

    animGroup = NULL;

    balls.resize(W * H);
    nextBalls.resize(W * H);

    //QGraphicsSvgItem tileBase(":/images/tile.svg");
    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            QGraphicsSvgItem *tile = new QGraphicsSvgItem(":/images/tile.svg");
            tile->setPos(x * CELL_SIDE, y * CELL_SIDE);
            tile->setZValue(0);
            graphicsScene->addItem(tile);
        }

    selectItem = new QGraphicsSvgItem(":/images/select.svg");
    selectItem->setZValue(1);
    graphicsScene->addItem(selectItem);

    gameMenu = menuBar()->addMenu("&Game");
    gameNew = new QAction("&New game", this);
    gameNewLines = new QAction("&Lines", this);
    gameNewSquares = new QAction("S&quares", this);
    gameNewBlocks = new QAction("&Blocks", this);
    gameExit = new QAction("&Exit", this);

    helpMenu = menuBar()->addMenu("&Help");
    helpAboutQt = new QAction("About &Qt", this);

    gameNew->setShortcut(QKeySequence("F2"));
    gameNewLines->setShortcut(QKeySequence("F3"));
    gameNewSquares->setShortcut(QKeySequence("F4"));
    gameNewBlocks->setShortcut(QKeySequence("F5"));

    gameMenu->addAction(gameNew);
    gameMenu->addSeparator();
    gameMenu->addAction(gameNewLines);
    gameMenu->addAction(gameNewSquares);
    gameMenu->addAction(gameNewBlocks);
    gameMenu->addSeparator();
    gameMenu->addAction(gameExit);
    helpMenu->addAction(helpAboutQt);

    setWindowTitle(tr("Lines-Qt"));
    setMinimumSize(CELL_SIDE * W, CELL_SIDE * H);

    connect(graphicsView, SIGNAL(mouseReleased(QMouseEvent*)), this, SLOT(processMouseEvent(QMouseEvent*)));
    connect(gameNew, SIGNAL(triggered()), this, SLOT(newGame()));
    connect(gameNewLines, SIGNAL(triggered()), this, SLOT(newGame()));
    connect(gameNewSquares, SIGNAL(triggered()), this, SLOT(newGame()));
    connect(gameNewBlocks, SIGNAL(triggered()), this, SLOT(newGame()));
    connect(gameExit, SIGNAL(triggered()), this, SLOT(close()));
    connect(helpAboutQt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    gameType = Lines;
    newGame();
}

MainWindow::~MainWindow()
{
    delete pGameField;
    delete pNextBalls;
	delete pDeletionMap;
}

void MainWindow::resizeEvent(QResizeEvent *event)
{
    QMainWindow::resizeEvent(event);
    //graphicsView->fitInView(graphicsScene->sceneRect(), Qt::KeepAspectRatio);
}

void MainWindow::processMouseEvent(QMouseEvent *event)
{
    if (locked)
        return;

    int x = event->x() / CELL_SIDE, y = event->y() / CELL_SIDE;

    if (x < 0 || y < 0 || x > W - 1 || y > H - 1)
        return;

#ifdef QT_DEBUG
	if (event->button() == Qt::RightButton)
	{
		int cell = pGameField->cell(x, y);
		cell++;
		cell %= pGameField->colors() + 1;
		pGameField->setCell(x, y, cell);

		int idx = x + W * y;
		if (balls[idx] != NULL)
		{
			graphicsScene->removeItem(balls[idx]);
            delete balls[idx];
			balls[idx] = NULL;
		}

		if (cell)
		{
			QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));
			ball->setPos(x * CELL_SIDE, y * CELL_SIDE);
			ball->setZValue(3);
			balls[idx] = ball;
			graphicsScene->addItem(ball);
		}
		return;
	}
#endif

    if (pGameField->cell(x, y))
    {
        selection = QPoint(x, y);
        selectItem->setPos(x * CELL_SIDE, y * CELL_SIDE);
        selectItem->setVisible(true);
        return;
    }

    Field pathMap(pGameField);
    if (pGameField->hasPath(selection.x(), selection.y(), x, y, &pathMap))
    {
        QGraphicsSvgItem *ball = balls[selection.x() + W * selection.y()];

        selectItem->setVisible(false);

        QLinkedList<QPoint> waypoints = pathMap.waypoints(x, y);
        QLinkedListIterator<QPoint> iter(waypoints);
#ifdef QT_DEBUG
        qDebug("(%d %d) -> (%d %d) (%d)", selection.x(), selection.y(), x, y, waypoints.count());
#endif

        animGroup = new QSequentialAnimationGroup(graphicsScene);
        //parGroup = new QParallelAnimationGroup(seqGroup);

        QPoint cur = iter.next();
        while (iter.hasNext())
        {
            QPoint next = iter.next();
            int diff = qAbs(next.x() - cur.x()) + qAbs(next.y() - cur.y());
            QPropertyAnimation *anim = new QPropertyAnimation(ball, "pos");
            anim->setDuration(diff * 25);
            anim->setStartValue(QPoint(cur.x() * CELL_SIDE, cur.y() * CELL_SIDE));
            anim->setEndValue(QPoint(next.x() * CELL_SIDE, next.y() * CELL_SIDE));
            animGroup->addAnimation(anim);

            //pathMarkers[next.x() + W * next.y()]->setVisible(true);

            cur = next;
        }

        pGameField->setCell(x, y, pGameField->cell(selection.x(), selection.y()));
        pGameField->setCell(selection.x(), selection.y(), 0);

        balls[x + W * y] = balls[selection.x() + W * selection.y()];
        balls[selection.x() + W * selection.y()] = NULL;

        selection = QPoint(-1, -1);

        lock();
        if (pGameField->deletionMap(pDeletionMap, gameType))
			connect(animGroup, SIGNAL(finished()), this, SLOT(removeLines()));
		else
			connect(animGroup, SIGNAL(finished()), this, SLOT(growBalls()));

        animGroup->start(QAbstractAnimation::DeleteWhenStopped);
    }
}

void MainWindow::lock()
{
    locked = true;
}

void MainWindow::unlock()
{
    animGroup = NULL;

    QString title;
    switch (gameType)
    {
    case Lines:
        title = "Lines";
        break;
    case Squares:
        title = "Squares";
        break;
    case Blocks:
        title = "Blocks";
        break;
    }
    title += QString(". Balls: %1; Score: %2").arg(QString::number(pGameField->count()), QString::number(score));

    this->setWindowTitle(title);
    locked = false;
}

void MainWindow::removeLines()
{
	//int animType = qrand() % 3;
    animGroup = new QParallelAnimationGroup(graphicsScene);

	int n = 0;

	for (int x = 0; x < W; x++)
		for (int y = 0; y < H; y++)
			if (pDeletionMap->cell(x, y))
			{
				int idx = x + W * y;
				pGameField->setCell(x, y, 0);
				n++;

				QPropertyAnimation *anim = new QPropertyAnimation(balls[idx], "pos");
				anim->setDuration(280);
				anim->setEndValue(QPoint((0.5 + x) * CELL_SIDE, (0.5 + y) * CELL_SIDE));
				animGroup->addAnimation(anim);

				anim = new QPropertyAnimation(balls[idx], "scale");
				anim->setDuration(280);
				anim->setEndValue((qreal)0.0);
				animGroup->addAnimation(anim);
			}

    switch (gameType)
    {
    case Lines:
        score += n * (n - 4);
        break;
    case Squares:
        score += n * (n - 3);
        break;
    case Blocks:
        score += n * (n - 6);
        break;
    }

	connect(animGroup, SIGNAL(finished()), this, SLOT(deleteGraphicItems()));

	animGroup->start(QAbstractAnimation::DeleteWhenStopped);
}

void MainWindow::deleteGraphicItems()
{
	for (int x = 0; x < W; x++)
		for (int y = 0; y < H; y++)
			if (pDeletionMap->cell(x, y))
			{
				int idx = x + W * y;
				graphicsScene->removeItem(balls[idx]);
				delete balls[idx];
				balls[idx] = 0;
			}

	showNextBalls();
}

void MainWindow::growBalls()
{
	QAnimationGroup *animGroup = new QParallelAnimationGroup(graphicsScene);

	int cell, idx = -1;

	// At first we look for a future ball at an occupied position
	// and move it them elsewhere if found.
	for (int x = 0; idx < 0 && x < W; x++)
		for (int y = 0; y < H; y++)
			if ((cell = pNextBalls->cell(x, y)) && pGameField->cell(x, y))
			{
				idx = x + W * y;
				break;
			}

	if (idx >= 0)
	{
		int n = 0;
		QPoint pts[W * H];
		for (int x = 0; x < W; x++)
			for (int y = 0; y < H; y++)
				if (!pNextBalls->cell(x, y) && !pGameField->cell(x, y))
				{
					pts[n] = QPoint(x, y);
					n++;
				}

		QPoint pt = pts[qrand() % n];
		pNextBalls->setCell(pt.x(), pt.y(), cell);
		pNextBalls->setCell(idx % W, idx / W, 0);
		QGraphicsSvgItem *ball = nextBalls[pt.x() + W * pt.y()] = nextBalls[idx];
		nextBalls[idx] = NULL;

#ifdef QT_DEBUG
		qDebug("Moving %d %d -> %d %d", idx % W, idx / W, pt.x(), pt.y());
#endif

		ball->setPos((0.3 + pt.x()) * CELL_SIDE, (0.3 + pt.y()) * CELL_SIDE);
	}

    // After that we transfer future balls to the game field and make animations for them.
    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            int cell = pNextBalls->cell(x, y);
			if (cell)
            {
                int idx = x + W * y;
				pGameField->setCell(x, y, cell);
                balls[idx] = nextBalls[idx];
                nextBalls[idx] = NULL;

                QPropertyAnimation *anim = new QPropertyAnimation(balls[idx], "pos");
                anim->setDuration(200);
                anim->setEndValue(QPoint(x * CELL_SIDE, y * CELL_SIDE));
                animGroup->addAnimation(anim);

                anim = new QPropertyAnimation(balls[idx], "scale");
                anim->setDuration(200);
                anim->setEndValue((qreal)1.0);
                animGroup->addAnimation(anim);
            }
        }
    pGameField->throwBalls(pNextBalls, 3);

    if (pGameField->deletionMap(pDeletionMap, gameType))
		connect(animGroup, SIGNAL(finished()), this, SLOT(removeLines()));
	else
		connect(animGroup, SIGNAL(finished()), this, SLOT(showNextBalls()));

    animGroup->start(QAbstractAnimation::DeleteWhenStopped);

}

void MainWindow::showNextBalls()
{
	QAnimationGroup *animGroup = NULL;

	for (int cell, idx, x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
			if (nextBalls[idx = x + W * y] == NULL && (cell = pNextBalls->cell(x, y)))
            {
				if (animGroup == NULL)
					animGroup = new QParallelAnimationGroup(graphicsScene);

				QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));

                ball->setScale(0.0);
                ball->setPos((0.5 + x) * CELL_SIDE, (0.5 + y) * CELL_SIDE);
                //ball->setOpacity(0.5);
                ball->setZValue(2);
                nextBalls[idx] = ball;
                graphicsScene->addItem(ball);

                QPropertyAnimation *anim = new QPropertyAnimation(ball, "pos");
                anim->setDuration(80);
                anim->setEndValue(QPoint((0.3 + x) * CELL_SIDE, (0.3 + y) * CELL_SIDE));
                animGroup->addAnimation(anim);

                anim = new QPropertyAnimation(ball, "scale");
                anim->setDuration(80);
                anim->setEndValue((qreal)0.4);
                animGroup->addAnimation(anim);
            }

	if (animGroup != NULL)
	{
		connect(animGroup, SIGNAL(finished()), this, SLOT(unlock()));

		animGroup->start(QAbstractAnimation::DeleteWhenStopped);
	}
	else
		unlock();
}

void MainWindow::newGame()
{
    QObject *pSender = sender();
#ifdef QT_DEBUG
    qDebug("Sender: %llx", (quint64)pSender);
#endif
    if (pSender == gameNewLines)
        gameType = Lines;
    else if (pSender == gameNewSquares)
        gameType = Squares;
    else if (pSender == gameNewBlocks)
        gameType = Blocks;

    if (animGroup != NULL)
    {
        disconnect(animGroup, SLOT(stop()));
        animGroup->stop();
    }

    int cell, idx;
    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            idx = x + W * y;
            if (pGameField->cell(x, y))
            {
                graphicsScene->removeItem(balls[idx]);
                delete balls[idx];
                balls[idx] = NULL;
            }
            if (pNextBalls->cell(x, y))
            {
                graphicsScene->removeItem(nextBalls[idx]);
                delete nextBalls[idx];
                nextBalls[idx] = NULL;
            }
        }

    pGameField->clear();
    pNextBalls->clear();

    pGameField->throwBalls(pGameField, 3);
    pGameField->throwBalls(pNextBalls, 3);

    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            idx = x + W * y;
            if ((cell = pGameField->cell(x, y)))
            {
                QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));
                ball->setPos(x * CELL_SIDE, y * CELL_SIDE);
                ball->setZValue(3);
                balls[idx] = ball;
                graphicsScene->addItem(ball);
            }
            else if ((cell = pNextBalls->cell(x, y)))
            {
                QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));

                ball->setScale(0.4);
                ball->setPos((0.3 + x) * CELL_SIDE, (0.3 + y) * CELL_SIDE);
                //ball->setOpacity(0.5);
                ball->setZValue(2);
                nextBalls[idx] = ball;
                graphicsScene->addItem(ball);
            }
        }

    selectItem->setVisible(false);
    selection = QPoint(-1, -1);
    score = 0;

    unlock();
}
